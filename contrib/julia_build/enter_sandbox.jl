using Sandbox, Pkg

rootfs_hash = Base.SHA1("5b0b851aca3c941b900a1301c13922c6cfc7f211")
rootfs_url = "https://github.com/JuliaCI/rootfs-images/releases/download/v5.26/package_linux.x86_64.tar.gz"

if !Pkg.Artifacts.artifact_exists(rootfs_hash)
    if !Pkg.Artifacts.download_artifact(rootfs_hash, rootfs_url; verbose=true)
        error("Unable to download artifact")
    end
end

config = SandboxConfig(
    # Read-only maps: the rootfs
    Dict(
        "/" => Pkg.Artifacts.artifact_path(rootfs_hash),
    ),
    # Read-write maps: this build directory
    Dict(
        "/build" => @__DIR__,
    ),
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
    # Map through these so we're interactive
    stdin,
    stderr,
    stdout,
)

# Start up a shell
with_executor() do exe
    run(exe, config, `/bin/bash`)
end
