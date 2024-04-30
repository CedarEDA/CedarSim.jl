using CedarSim
using Documenter
using OrdinaryDiffEq
using UnicodePlots

makedocs(
    sitename = "CedarSim",
    format = Documenter.HTML(),
    modules = [CedarSim],
    pages = Any[
        "Home" => "index.md",
        "circuits.md",
        "devices.md",
    ],
    warnonly = true,
)

deploydocs(
    repo = "github.com/CedarEDA/CedarSim.jl.git",
)
