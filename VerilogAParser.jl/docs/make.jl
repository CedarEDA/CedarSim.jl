using Documenter
using VerilogAParser

# Normalize the documenter key if it's not already base64-encoded
using Base64
documenter_key = get(ENV, "DOCUMENTER_KEY", "")
try
    base64decode(documenter_key)
catch e
    if isa(e, ArgumentError)
        if !endswith(documenter_key, "\n")
            global documenter_key = string(documenter_key, "\n")
        end
        ENV["DOCUMENTER_KEY"] = base64encode(documenter_key)
    else
        rethrow(e)
    end
end

makedocs(
    sitename = "VerilogAParser",
    format = Documenter.HTML(),
    modules = [VerilogAParser],
    warnonly = true,
)

deploydocs(
    repo = "github.com/JuliaComputing/CedarSim.jl.git",
    branch = "docs-verilogaparser",
)
