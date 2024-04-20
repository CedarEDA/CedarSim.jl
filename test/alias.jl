using CedarSim
using CedarSim.SpectreEnvironment
using Test

ckt = """
subckt myres pos neg
    parameters r=1k
    r1 (pos neg) resistor r=r
ends myres

x1 (vcc 0) myres r=2k
v1 (vcc 0) vsource dc=1
"""

circuit_code = CedarSim.make_spectre_circuit(
    CedarSim.SpectreNetlistParser.parse(ckt),
);
fn = eval(circuit_code);

# test aliasmap
map = CedarSim.aliasmap(fn)
ref = Dict{CedarSim.DScope, CedarSim.DScope}(
    CedarSim.DScope(CedarSim.DScope(CedarSim.DScope(), :x1), :node_pos) => CedarSim.DScope(CedarSim.DScope(), :node_vcc),
    CedarSim.DScope(CedarSim.DScope(CedarSim.DScope(), :x1), :node_neg) => CedarSim.DScope(CedarSim.DScope(), :node_0),
)
@test map == ref

# test alias in circuit
include("common.jl")
sys, sol = solve_circuit(fn)

@test all(isapprox.(sol[sys.x1.node_pos], sol[sys.node_vcc]))
@test all(isapprox.(sol[sys.x1.node_neg], sol[sys.node_0]))
