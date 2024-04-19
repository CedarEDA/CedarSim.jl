using CedarSim
using CedarSim.SpectreEnvironment
using SpectreNetlistParser
using GF180MCUPDK

include("../common.jl")

import BSIM4
const bsim4 = load_VA_model(BSIM4.bsim4_va)

path = joinpath(@__DIR__, "gf180ref.spice")
sa2 = SpectreNetlistParser.parsefile(path);
code2 = CedarSim.make_spectre_circuit(sa2, [], []);
fn2 = eval(code2)
# fn()

p2 = CedarSim.ParamObserver()
fn2(p2)
m2 = p2.x0.m0
param2 = CedarSim.modelparams(m2)

debug_gf = DAECompiler.DebugConfig(
    ir_levels = DAECompiler.IRCodeRecords(),
    ss_levels = DAECompiler.SystemStructureRecords(),
    verify_ir_levels=true,
    ir_log = joinpath(@__DIR__, "..", "ir", "sky130", "gf180"))

circuit = CedarSim.DefaultSim(fn2)
time_bounds = (0.0, 2e-3)
sys = CircuitIRODESystem(circuit; debug_config=debug_gf)
prob = DAEProblem(sys, nothing, nothing, time_bounds, circuit)
sol = solve(prob, IDA(); reltol=1e-6, abstol=1e-6, initializealg=CedarDCOp())

# this takes the result of parse_unified and runs the gf180 circuit with the sky130 parameters
# useful for ruling out parsing or model errors
if @isdefined param
    for f in fieldnames(typeof(param))
        sky = getfield(param, f)
        gf = getfield(param2, f)
        if sky != gf
            println("$f: $sky $gf")
        end
    end

    @eval function fn3()
        $fn2(CedarSim.ParamLens((;x0=(;m0=$param))))
    end

    debug_gf_sky = DAECompiler.DebugConfig(
        ir_levels = DAECompiler.IRCodeRecords(),
        ss_levels = DAECompiler.SystemStructureRecords(),
        # verify_ir_levels=true,
        ir_log = joinpath(@__DIR__, "..", "ir", "sky130", "gf180_sky"))

    circuit = CedarSim.DefaultSim(fn3)
    time_bounds = (0.0, 2e-3)
    sys = CircuitIRODESystem(circuit; debug_config=debug_gf_sky)
    prob = DAEProblem(sys, nothing, nothing, time_bounds, circuit)
    sol = solve(prob, IDA(); reltol=1e-6, abstol=1e-6, initializealg=CedarDCOp())
end
