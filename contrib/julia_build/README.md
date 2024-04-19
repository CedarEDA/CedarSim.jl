# CedarSim julia build

This makefile contains the steps required to build an `x86_64-linux-gnu` build of Julia required for CedarSim.
It is intended to be run periodically once a new version of Julia known to be compatible with the latest versions of CedarSim, DAECompiler, etc.. is identified.
It uploads to an S3 bucket which is then downloaded from in the CI builds.
For interactive use, we suggest running `juliaup_cedar.sh` to setup various juliaup links to a locally-downloaded version of the same build.
While there is preliminary support for maintaining multiple separate 'flavors' of a julia build here, most developers should just stick to the defaults.

# Using the buildkite pipeline to build a new version

Open a PR where you edit the `julia_version.inc` file to select a new version, and the PR will build that Julia version, and use it as the "blessed" version within that PR.
If you are satisfied with that change, merge the pull request and that version will be blessed as the new latest Cedar-blessed Julia version!

# Manually building and uploading a new version

First, you will probably want to run at least the `make` step from within a sandbox so that you're linking against an old version of glibc.
To do so, run `julia enter_sandbox.jl`.

Next, edit `julia_version.inc`:
```
$ cat julia_version.inc 
JULIA_GITSHA := 2ed1cfeb01204a1997182cad0a7f268bf8320f3d
```

Next, run `make -j$(nproc)` to build the tarball.
Once that's done, you can exit the sandbox and run `make upload`:
```
$ make upload
aws s3 cp julia/julia-2ed1cfeb01-linux64.tar.gz s3://jc-cedarsim-juliabuilds/vanilla/julia-2ed1cfeb01-linux64.tar.gz
upload: julia/julia-2ed1cfeb01-linux64.tar.gz to s3://jc-cedarsim-juliabuilds/vanilla/julia-2ed1cfeb01-linux64.tar.gz
```

NOTE: You will need an AWS S3 key installed for this to work.
If you need one, contact `@staticfloat` on slack, but most people should not require this.

After testing that the uploaded tarball works for you, set it to be the latest via `make upload-latest`.
