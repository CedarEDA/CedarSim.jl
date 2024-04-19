module bsimcmg_spectre

using CedarSim
using CedarSim.VerilogAParser
using OrdinaryDiffEq
using UnicodePlots
using Sundials
using SpectreNetlistParser
using CedarSim.SpectreEnvironment
using Test
using SciMLBase
import Pkg, CMC

const bsimcmg_path = joinpath(Pkg.pkgdir(CMC), "cmc_models/bsimcmg107/bsimcmg.va")
const bsimcmg = load_VA_model(bsimcmg_path)

sa = SpectreNetlistParser.parsefile(joinpath(dirname(pathof(SpectreNetlistParser)), "../test/examples/7nm_TT.scs"));
eval(CedarSim.make_spectre_netlist(sa))

sa2 = SpectreNetlistParser.parsefile(joinpath(@__DIR__, "asap7_inv.scs"));
code = CedarSim.make_spectre_circuit(sa2)
circuit = eval(code)
# circuit()

sys = CircuitIRODESystem(circuit)

prob = DAEProblem(sys, nothing, nothing, (0.0, 1e-7); initializealg=CedarDCOp())
integ = init(prob, IDA(); abstol=1e-8)
sol = solve(prob, IDA())

# We expect the out node to be positive after dc init. It is possible for this
# node to go negative, but that requires capacitive effects, which are off in
# DC. A naive initialization of the DAE would give a non-DC solution.
@test integ[sys.node_Vout] > 0.
@test sol.retcode == SciMLBase.ReturnCode.Success

end # module bsimcmg_spectre
