# CedarSim.jl
🌶 Very SPICE-y

# Setup

First, setup the blessed version of julia via running
`bash contrib/julia_build/juliaup_cedar.sh` to get a `+cedar` channel available in [juliaup](https://github.com/JuliaLang/juliaup).
Then start julia via `julia +cedar --project=.` to run that blessed release with `CedarSim` as the current project.

For more information on modifying the blessed version of Julia see the documentation in in [contrib/julia_build/](contrib/julia_build/).


## Development

Next, setup a project with the following packages dev’ed out:
* `https://github.com/JuliaDiff/Diffractor.jl`
* `https://github.com/JuliaComputing/DAECompiler.jl`
* `https://github.com/SciML/ModelingToolkit.jl`
* `git@github.com:JuliaComputing/CedarSim.jl.git`
* `git@github.com:JuliaComputing/BSIM4.va.git`

You will probably want to put something like the following in `~/.julia/config/startup.jl`
```julia
ENV["JULIA_PKG_USE_CLI_GIT"] = "true" # avoid weird permission errors
ENV["JULIA_PKG_SERVER"] = "" # optional, skip pkg cdn and use git
using Revise # avoid restarting the repl as much
```

And disable precompilation of the project you're actively working on.
If you're updating dependencies a lot, you might also want to disable some other packages that take a long time.

`LocalPreferences.toml` example when working on CedarSim:
```toml
[SnoopPrecompile]
skip_precompile = ["OrdinaryDiffEq", "CedarSim"]
```

This file can also be edited with `set_preferences!(SnoopPrecompile, "skip_precompile" => pkg_names)`

Development buils of Julia frequently break VSCode integration, so it's often not possible to use the integrated REPL.
A hack to give this a slightly better chance of working is to go to settings and set the julia binary to point to the following script.
This uses release julia for the language server and cedar julia for running your code.
See https://github.com/julia-vscode/julia-vscode/issues/3160

```bash
#!/usr/bin/env bash
set -e
if [ -z "$JULIA_LANGUAGESERVER" ]; then
	exec julia +cedar "$@"
else
	exec julia +release "$@"
fi
```
