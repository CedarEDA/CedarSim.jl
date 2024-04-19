module unified
using CedarSim
using CedarSim.SpectreEnvironment
using SpectreNetlistParser

include("../common.jl")

using BSIM4
import Sky130PDK

sky_dir = joinpath(pkgdir(Sky130PDK), "sky130A/libs.tech/ngspice")
path = joinpath(@__DIR__, "scale.spice")
sa2 = SpectreNetlistParser.parsefile(path);
code2 = CedarSim.make_spectre_circuit(sa2, [], [sky_dir]);
fn = eval(code2)
fn()

p = CedarSim.ParamObserver()
fn(p)
m = p.x0.msky130_fd_pr__nfet_01v8
param = CedarSim.modelparams(m)

debug_sky = DAECompiler.DebugConfig(
    ir_levels = DAECompiler.IRCodeRecords(),
    ss_levels = DAECompiler.SystemStructureRecords(),
    verify_ir_levels=true,
    ir_log = joinpath(@__DIR__, "..", "ir", "sky130", "sk130"))

circuit = CedarSim.DefaultSim(fn)
time_bounds = (0.0, 2e-3)
sys = CircuitIRODESystem(circuit; debug_config=debug_sky)
prob = DAEProblem(sys, nothing, nothing, time_bounds, circuit)
sol = solve(prob, IDA(); reltol=1e-12, abstol=1e-12, initializealg=CedarDCOp())

# this takes the result of parse_ref and runs the sky130 circuit with the gf180 parameters
# useful for ruling out parsing or model errors
if @isdefined param2
    @eval function fn4()
        $fn(CedarSim.ParamLens((;x0=(;msky130_fd_pr__nfet_01v8=$param2))))
    end

    debug_sky_gf = DAECompiler.DebugConfig(
        ir_levels = DAECompiler.IRCodeRecords(),
        ss_levels = DAECompiler.SystemStructureRecords(),
        # verify_ir_levels=true,
        ir_log = joinpath(@__DIR__, "..", "ir", "sky130", "sk130_gf"))

    circuit = CedarSim.DefaultSim(fn4)
    time_bounds = (0.0, 2e-3)
    sys = CircuitIRODESystem(circuit; debug_config=debug_sky_gf)
    prob = DAEProblem(sys, nothing, nothing, time_bounds, circuit)
    sol = solve(prob, IDA(); reltol=1e-6, abstol=1e-6, initializealg=CedarDCOp(), progress=true )
end
end