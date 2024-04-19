using Pkg, Pkg.Artifacts, SHA, TreeArchival, VerilogAParser

function lookup_artifact_hash(name)
    artifacts_toml = find_artifacts_toml(Base.pkgdir(VerilogAParser))
    if artifacts_toml === nothing
        return nothing
    end

    return artifact_hash(name, artifacts_toml)
end

Base.bytes2hex(::Nothing) = ""
Base.bytes2hex(h::Base.SHA1) = bytes2hex(h.bytes)
function build_and_upload_artifact(name, version=Base.pkgversion(VerilogAParser))
    local_hash = treehash(joinpath(@__DIR__, name))
    recorded_hash = lookup_artifact_hash(name)
    if local_hash != recorded_hash
        @info(
            "New $(name) content detected, packaging into an artifact",
            local_hash = bytes2hex(local_hash),
            recorded_hash = bytes2hex(recorded_hash),
        )
        filename = "$(name)-$(version)-$(bytes2hex(local_hash)[end-8:end]).tar.gz"
        filepath = joinpath(@__DIR__, filename)
        rm(filepath; force=true)
        archive(
            joinpath(@__DIR__, name),
            filepath,
            "gzip";
            compression_level=9,
        )
        tarball_hash = bytes2hex(open(sha256, filepath))
        url = "https://jc-cedarsim-juliabuilds.s3.amazonaws.com/artifacts/$(filename)"

        # TODO upload this automatically
        @info("Please upload", filepath, url)

        # Bind new entry in Artifacts.toml
        Pkg.Artifacts.bind_artifact!(
            find_artifacts_toml(Base.pkgdir(VerilogAParser)),
            name,
            Base.SHA1(local_hash);
            download_info=[
                (url, tarball_hash),
            ],
            force=true
        )
    end
end

build_and_upload_artifact("vams")
