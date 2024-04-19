module bsimcmg_inverter
using Test
using VerilogAParser
include("../common.jl")

import ASAP7PDK

path = joinpath(@__DIR__, "inverter_cmg_cedar.cir")
ast = CedarSim.SpectreNetlistParser.SPICENetlistParser.SPICENetlistCSTParser.parsefile(path);
circuit_code = CedarSim.make_spectre_circuit(
    ast,
    # Automatically allow including things in the same directory
    String[dirname(path)],
);
circuit = eval(circuit_code);
circuit()
pcir = DefaultSim(circuit)
sys = CircuitIRODESystem(pcir)
prob = DAEProblem(sys, nothing, nothing, (0.0, 4e-7), pcir; initializealg=CedarDCOp(;abstol=1e-10))
sol = solve(prob, IDA(); reltol=1e-7, abstol=1e-7, initializealg=CedarTranOp())

@test sol.retcode == ReturnCode.Success
end # module bsimcmg_inverter
