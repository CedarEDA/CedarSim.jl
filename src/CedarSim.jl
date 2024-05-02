module CedarSim

using DiffEqBase
using DynamicScope
using VectorPrisms
using Base: @with, with

# re-exports
export DAEProblem
export @dyn, @requires, @provides, @isckt_or
export solve

include("util.jl")
include("vasim.jl")
include("simulate_ir.jl")
include("simpledevices.jl")
include("spectre_env.jl")
include("circuitodesystem.jl")
include("spectre.jl")
include("va_env.jl")
include("sweeps.jl")
include("dcop.jl")
include("ac.jl")
include("ModelLoader.jl")
include("aliasextract.jl")
include("netlist_utils.jl")
include("deprecated.jl")
include("circsummary.jl")

import .ModelLoader: load_VA_model
export load_VA_model

# Store the known-good julia version that we should be compiling against
_blessed_julia_version = begin
    julia_version_path = joinpath(@__DIR__, "../contrib/julia_build/julia_version.inc")
    Base.include_dependency(julia_version_path)
    strip(split(String(read(julia_version_path)), ":=")[2])
end

function check_version_match()
    # Only print this out if we're not precompiling, as it's annoying to see this
    # pop up for every extension precompile process.
    if _blessed_julia_version != Base.GIT_VERSION_INFO.commit && ccall(:jl_generating_output, Cint, ()) != 1
        @warn("""
        You are not running on the Cedar-blessed Julia version! (currently '$(_blessed_julia_version)')
        Try running './juliaup_cedar.sh', and remember to start julia with `julia +cedar`!
        """)
    end
end

function __init__()
    # Do this at `__init__()` time instead of precompile time because it's too easy to miss it during precompilation.
    check_version_match()
end

using PrecompileTools
@setup_workload let
    spice = """
    * my circuit
    v1 vcc 0 DC 5
    r1 vcc n1 1k
    l1 n1 n2 1m
    c1 n2 0 1u
    """
    spectre = """
    c1 (Y 0) capacitor c=100f
    r2 (Y VDD) BasicVAResistor R=10k
    v1 (VDD 0) vsource type=dc dc=0.7_V
    """
    @compile_workload @time begin
        sa1 = VerilogAParser.parsefile(joinpath(@__DIR__, "../VerilogAParser.jl/test/inputs/resistor.va"))
        code1 = CedarSim.make_module(sa1)
        sa2 = SpectreNetlistParser.parse(IOBuffer(spectre))
        code2 = CedarSim.make_spectre_circuit(sa2)
        sa3 = SpectreNetlistParser.parse(IOBuffer(spice); start_lang=:spice)
        code3 = CedarSim.make_spectre_circuit(sa3)
    end
end

end # module
