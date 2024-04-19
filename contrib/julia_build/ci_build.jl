using HTTP

# First, check to see if the current-blessed version is already uploaded:
cd(@__DIR__)
julia_version = strip(split(String(read("julia_version.inc")), ":=")[2])
short_julia_version = julia_version[1:10]
OS = Sys.isapple() ? "macos" : Sys.islinux() ? "linux" : error("OS not supported")
ARCH = Sys.ARCH in (:x86_64, :aarch64) ? string(Sys.ARCH) : error("ARCH not supported")

println("--- Checking S3 bucket")

s3_url = "https://jc-cedarsim-juliabuilds.s3.amazonaws.com/vanilla/julia-$(short_julia_version)-$(OS)-$(ARCH).tar.gz"
h = HTTP.head(s3_url; status_exception = false)
if h.status == 200
    @info("Blessed Julia version up to date!", short_julia_version)
    if Sys.islinux()
        run(addenv(`buildkite-agent pipeline upload ./buildkite_pipeline_insert_env.yml`, Dict("CEDAR_BLESSED_URL" => s3_url)))
    end

    # If it already exists, but has a different ETag than the "latest" build, (and we're on the default branch)
    # then promote it to the current `latest`:
    if get(ENV, "BUILDKITE_BRANCH", "") == get(ENV, "BUILDKITE_PIPELINE_DEFAULT_BRANCH", "main")
        latest_url = "https://jc-cedarsim-juliabuilds.s3.amazonaws.com/vanilla/julia-latest-$(OS)-$(ARCH).tar.gz"
        hl = HTTP.head(latest_url; status_exception = false)

        function etag_or_nothing(h)
            etag_headers = [v for (k, v) in h.headers if k == "ETag"]
            if isempty(etag_headers)
                return nothing
            end
            return only(etag_headers)
        end

        h_etag = only(v for (k, v) in h.headers if k == "ETag")
        hl_etag = etag_or_nothing(hl)
        if h_etag != hl_etag
            @info("Promoting to latest build", short_julia_version)
            s3_src = "s3://jc-cedarsim-juliabuilds/vanilla/julia-$(short_julia_version)-$(OS)-$(ARCH).tar.gz"
            s3_dst = "s3://jc-cedarsim-juliabuilds/vanilla/julia-latest-$(OS)-$(ARCH).tar.gz"
            run(`aws s3 cp $(s3_src) $(s3_dst)`)
        end
    end
    exit(0)
end

# If it's not, let's build it:
if OS == "linux"
    println("--- Preparing build environment")
    using Sandbox, Pkg, Scratch
    rootfs_hash = Base.SHA1("b62e325b7df660c88a898ce5b655309a005ba690")
    rootfs_url = "https://github.com/JuliaCI/rootfs-images/releases/download/v7.0/package_linux.x86_64.tar.gz"

    if !Pkg.Artifacts.artifact_exists(rootfs_hash)
        if !Pkg.Artifacts.download_artifact(rootfs_hash, rootfs_url; verbose=true)
            error("Unable to download artifact")
        end
    end

    config = SandboxConfig(
        Dict("/" => Pkg.Artifacts.artifact_path(rootfs_hash)),
        Dict("/build" => @__DIR__),
        # Environment mapping
        Dict(
            "PATH" => "/usr/local/bin:/usr/bin:bin",
            "HOME" => "/home/juliaci",
            "USER" => "juliaci",
        );
        # Run as `juliaci` within the sandbox; if we're using
        # unprivileged namepsaces (which is the default) we'll still
        # get files owned by our own user on the outside of the sandbox.
        uid = 1000,
        gid = 1000,
        # Start the shell in `/build`
        pwd = "/build",
        stderr,
        stdout,
    )

    println("--- Build")
    @info("Building", version=short_julia_version, threads=Sys.CPU_THREADS+1)
    with_executor() do exe
        run(exe, config, `/bin/bash -c "make -j$(Sys.CPU_THREADS+1)"`)
    end
else # OS == "macos"
    println("--- Build")
    run(`/bin/bash -c "make -j$(Sys.CPU_THREADS+1)"`)
end # Sys.islinux() / Sys.isapple()

println("--- Upload")
filename = "julia-$(short_julia_version)-$(OS)-$(ARCH).tar.gz"
cd("julia") do
    # Now that we've built the tarball, upload it to buildkite:
    run(`buildkite-agent artifact upload $(filename)`)

    # Also upload it to S3:
    run(`aws s3 cp $(filename) s3://jc-cedarsim-juliabuilds/vanilla/$(filename)`)
end

if Sys.islinux()
    # Set CEDAR_BLESSED_URL to the S3 url for all subsequent jobs
    run(addenv(`buildkite-agent pipeline upload ./buildkite_pipeline_insert_env.yml`, Dict("CEDAR_BLESSED_URL" => s3_url)))
end
