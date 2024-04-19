module ddx_tests

include("common.jl")

const NLVCR = load_VA_model(joinpath(@__DIR__, "NLVCR.va"))

function VRcircuit()
    vcc = Named(net, "vcc")()
    vg = Named(net, "vg")()
    gnd = Named(net, "gnd")()
    Named(V(5.), "V1")(vcc, gnd)
    Named(V(3.), "V2")(vg, gnd)
    Named(NLVCR(CedarSim.mknondefault(2.)), "R")(vcc, vg, gnd)
    Gnd()(gnd)
end

VRcircuit()

sys, sol = solve_circuit(VRcircuit)

@test sol[sys.V1.I][end] == -5*2*2*3

end # module ddx_tests
