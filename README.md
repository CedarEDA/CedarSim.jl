<a name="logo"/>
<div align="center">
<img src="docs/img/cedar_sim.svg" alt="CedarSim Logo"></img>
</div>
</a>

<a href="https://help.juliahub.com/cedarsim/dev/"><img src='https://img.shields.io/badge/docs-dev-blue.svg'/></a> 🌶 Very SPICE-y

> [!WARNING]
> The public release of Cedar is ongoing. You are welcome to look around, but things will be unstable and various pieces may be missing. If you are not feeling adventurous, please check back in a few weeks.

CedarSim is a modern, high-performance analog circuit simulator written in
the Julia programming language.

CedarSim is part of the full [Cedar EDA](https://github.com/CedarEDA) EDA platform. This package contains the implementation and documentation of the simulator component *only*. If you are an end user, please see https://help.juliahub.com/cedareda/ to get started.

In the current version of CedarSim, you can expect the following features:

- Import of multi-dialect SPICE/Spectre netlists
- Import of Verilog-A models
- Transient/AC/Noise analyses
- Integration with the broader Julia and SciML stack
- Full differentiability (for sensitivites, optimization, ML, etc.)
- Very high performance (expected to be on par with leading commercial simulators)

However, please note that CedarSim is still early release software. Please be aware of the following limitations (we're actively working on them, they are immuturity constraints, not design restrictions):

- CedarSim aggressively specializes and generates optimized code for every circuit being simulated. This process can take a substantial amount of time. Furthermore, the results of this process are currently not cached. As a result, simulation latency is high (often exceeding simulation time, unless you are running sweeps) and compilation time can be prohibitive for larger circuits. We are very actively working on this issue and expect it to be resolved soon.

- Model coverage of builtin SPICE models is limited. If you run into a missing model or unimplemented model parameters, please let us know which devices are most important to your work. We will prioritize missing models according to public demand.

- SPICE dialect compatibility may be limited. `ngspice` dialect compatibility is most mature, but not complete. If Cedar rejects your netlist that works in another simulator, please file an issue, so we can adjust the parser.

- GPU execution support is not yet enabled in the released version of Cedar.

- Transient noise analysis is not yet available

- Some analyses may be prohibitively slow or memory-intensive, we are always interested in building up more test cases for our internal benchmarks for prioritization.

# License / Contributing

The Cedar EDA platform is dual-licensed under a commercial license and CERN-OHL-S v2. In addition, some packages (including this one) are also available under the MIT license. Please see the LICENSE file for more
information and the LICENSE.FAQ.md file for more information on how to
use Cedar under the CERN-OHL-S v2 license.

We are accepting PRs on all Cedar repositories, although you must sign the Cedar Contributor License Agreement (CLA) for us to be able to incorporate your changes into the upstream repository. Additionally, if you would like to make major or architectural changes, please discuss this with us *before* doing the work. Cedar is a complicated piece of software, with many moving pieces, not all of which are publicly available. Because of this, we may not be able to take your changes, even if they are correct and useful (so again, please talk to us first).

# Getting started

There are several ways to get Cedar:

- As part of the commercial Cedar product
- As part of JuliaHub's Cedar open source silicon offering on juliahub.com
- By downloading a pre-compiled binary (coming soon)
- By using Cedar as a julia package

For end users who are primarily analog designers, we recommend the hosted
version on juliahub.com (preferably) or the pre-compiled binaries. These versions undergo additional manual QA, have known-compatible versions of other parts of the Julia ecosystem and are compiled into a single system
image for a slightly snappier experience.

## Using CedarSim as a julia package
For those who are interested in building on top of Cedar, have
more complicated use cases or want to contribute to Cedar, the julia package workflow is recommended. Please see https://pkgdocs.julialang.org/v1/ for documentation on using the julia package manager. All Cedar packages are managed through the [Cedar public registry](https://github.com/CedarEDA/PublicRegistry/).

Once you have added the Cedar public registry, obtaining CedarSim should be as easy as:

```
pkg> add CedarSim
```

However, please note that Cedar makes heavy use of julia compiler internals,
and a pre-release version of Julia may be required. We keep a record of which julia version (knwon as `Cedar blessed`) is currently known to work well with the current version of Cedar and provide special binary builds for this version (which is also used for pre-built workflows mentioned above).

You may set up the blessed version of julia via running
`bash contrib/julia_build/juliaup_cedar.sh` to get a `+cedar` channel available in [juliaup](https://github.com/JuliaLang/juliaup).
Then start julia via `julia +cedar --project=.` to run that blessed release with `CedarSim` as the current project.

For more information on modifying the blessed version of Julia see the documentation in in [contrib/julia_build/](contrib/julia_build/).

## Additional development tips

CedarSim has long precompilation times. You may disable precompilation of the project you're actively working on.
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
