module demo_bsimcmg

using CedarSim
using CedarSim.VerilogAParser
using Sundials
using OrdinaryDiffEq
using DAECompiler
# optional for this test, but the test gets harder when it's enabled, so let's keep it
using ChainRules
using Test

using ASAP7PDK.asap7_tt

const R = CedarSim.SimpleResistor
const C = CedarSim.SimpleCapacitor
const t = CedarSim.SpectreEnvironment.var"$time"
const V(v) = CedarSim.VoltageSource(dc=v)

function circuit()
    vcc = Named(net, "vcc")()
    gnd = Named(net, "gnd")()
    out = Named(net, "out")()
    #println(t())
    Named(V(1.), "V1")(vcc, gnd)
    Named(nmos_lvt(), "M1")(out, vcc, gnd, gnd)
    Named(R(1000.), "R1")(out, gnd)
    Named(Gnd(), "gnddev")(gnd)
end

sys = CircuitIRODESystem(circuit);
prob = DAEProblem(sys, zeros(3), zeros(3), (0.0, 1e-5); initializealg=CedarTranOp());
sol = solve(prob, IDA())

#@test sol[sys.M1.GM][end] > 0

end # module demo_bsimcmg
