using Documenter
using SpectreNetlistParser

makedocs(
    sitename = "SpectreNetlistParser",
    format = Documenter.HTML(),
    modules = [SpectreNetlistParser],
    warnonly = true,
)

deploydocs(
    repo = "github.com/CedarEDA/CedarSim.jl.git",
    branch = "gh-pages-spectrenetlistparser",
)
