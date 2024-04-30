using Documenter
using VerilogAParser

makedocs(
    sitename = "VerilogAParser",
    format = Documenter.HTML(),
    modules = [VerilogAParser],
    warnonly = true,
)

deploydocs(
    repo = "github.com/CedarEDA/CedarSim.jl.git",
    branch = "gh-pages-verilogaparser",
)
