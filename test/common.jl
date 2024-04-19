using CedarSim
using CedarSim.SpectreEnvironment
using SpectreNetlistParser
using Test
using OrdinaryDiffEq
using SciMLBase
using DAECompiler
using Random
using Sundials

# These must be top-level `const` values, otherwise `DAECompiler` doesn't know
# that we're not about to redefine it halfway through the model, and some of our
# static assumptions break.  We have a test asserting this in `test/compilation.jl`.
const R = CedarSim.SimpleResistor
const C = CedarSim.SimpleCapacitor
const L = CedarSim.SimpleInductor
const V(v) = CedarSim.VoltageSource(dc=v)
const I(i) = CedarSim.CurrentSource(dc=i)
const sim_time = CedarSim.DAECompiler.sim_time

const deftol = 1e-8

# Our default tolerances are one order of magnitude above our default solve tolerances
isapprox_deftol(x, y) = isapprox(x, y; atol=deftol*10, rtol=deftol*10)
isapprox_deftol(x) = y->isapprox(x, y; atol=deftol*10, rtol=deftol*10)

allapprox_deftol(itr) = isempty(itr) ? true : all(isapprox_deftol(first(itr)), itr)

"""
    solve_circuit(circuit::Function; time_bounds = (0.0, 1.0), reltol, abstol, u0)

Solves a circuit provided in the form of a function. Uses default values for various
pieces of the solver machinery, override them with the appropriate kwargs.
Returns `sys`, `sol`.
"""
function solve_circuit(circuit::CedarSim.AbstractSim; time_bounds::Tuple = (0.0, 1.0),
                       reltol=deftol, abstol=deftol, u0=nothing, debug_config = (;))
    sys = CircuitIRODESystem(circuit; debug_config)
    prob = ODEProblem(sys, u0, time_bounds, circuit)
    sol = solve(prob, FBDF(autodiff=false); reltol, abstol, initializealg=CedarTranOp(;abstol=1e-14))
    return sys, sol
end
solve_circuit(circuit; kwargs...) = solve_circuit(CedarSim.DefaultSim(circuit); kwargs...)

"""
    solve_spice_file(spice_file; kwargs...)

Read in the given SPICE file, generate a CircuitIRODESystem, and pass it off to `solve_circuit()`
"""
function solve_spice_file(spice_file::String; include_dirs::Vector{String} = String[dirname(spice_file)], kwargs...)
    circuit_code = CedarSim.make_spectre_circuit(
        CedarSim.SpectreNetlistParser.SPICENetlistParser.SPICENetlistCSTParser.parsefile(spice_file),
        include_dirs,
    );
    circuit = eval(circuit_code)
    invokelatest(circuit)
    return solve_circuit(circuit; kwargs...)
end

function solve_spice_code(spice_code::String; include_dirs::Vector{String} = String[], kwargs...)
    circuit_code = CedarSim.make_spectre_circuit(
        CedarSim.SpectreNetlistParser.SPICENetlistParser.SPICENetlistCSTParser.parse(spice_code),
        include_dirs,
    );
    fn = eval(circuit_code)
    invokelatest(fn)
    return solve_circuit(fn; kwargs...)
end


"""
    solve_spectre_file(spice_file; kwargs...)

Read in the given Spectre file, generate a CircuitIRODESystem, and pass it off to `solve_circuit()`
"""
function solve_spectre_file(spectre_file::String; include_dirs::Vector{String} = String[dirname(spectre_file)], kwargs...)
    circuit_code = CedarSim.make_spectre_circuit(
        CedarSim.SpectreNetlistParser.parsefile(spectre_file),
        include_dirs,
    );
    circuit = eval(circuit_code)
    invokelatest(circuit)
    return solve_circuit(circuit; kwargs...)
end

function solve_spectre_code(spectre_code::String; include_dirs::Vector{String} = String[], kwargs...)
    circuit_code = CedarSim.make_spectre_circuit(
        CedarSim.SpectreNetlistParser.parse(spectre_code),
        include_dirs,
    );
    fn = eval(circuit_code);
    invokelatest(fn)
    return solve_circuit(fn; kwargs...)
end

#=
# This is a useful define for ensuring that our SPICE simulations are giving reasonable answers
using NgSpice
function ngspice_simulate(spice_file::String, net::String; time_bounds=(0.0, 1.0))
    NgSpice.source(spice_file)
    NgSpice.cmd("tran $((time_bounds[2] - time_bounds[1])/1000) $(time_bounds[2])")
    return NgSpice.getvec(net)[3]
end
=#
