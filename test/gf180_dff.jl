module gf180
using CedarSim
using BSIM4
using GF180MCUPDK
using CedarSim.SpectreEnvironment
using Sundials
using LinearAlgebra
using Test

include(joinpath(Base.pkgdir(CedarSim), "test", "common.jl"))
const bsim4 = load_VA_model(BSIM4.bsim4_va)

using CedarSim.SpectreNetlistParser: SPICENetlistParser
repo_dir = dirname(dirname(Base.pathof(CedarSim)))
dffdir = joinpath(repo_dir, "test", "DFF")
sa1 = SPICENetlistParser.parsefile(joinpath(dffdir, "DFF_cap_all.cir"));
code = CedarSim.make_spectre_circuit(sa1, [dffdir]);
circuit = eval(code)

sys = CircuitIRODESystem(circuit; debug_config=(;store_ir_levels=true, verify_ir_levels=true))

# We crank up `abstol` during initialization to help prevent strange
# initializations with floating/metastable nets at t=0.
prob = DAEProblem(sys, nothing, nothing, (0.0, 7e-7); initializealg=CedarDCOp(;abstol=1e-14));
sol = solve(prob, IDA(), abstol=1e-5, initializealg=CedarDCOp(;abstol=1e-14));

@testset "gf180" begin
    @test sol.retcode == SciMLBase.ReturnCode.Success
    @test only(sol(1.5e-7, idxs=[sys.node_q])) ≈ 0.0 atol=1e-4
    @test only(sol(2.5e-7, idxs=[sys.node_q])) ≈ 0.0 atol=1e-4
    @test only(sol(4.5e-7, idxs=[sys.node_q])) ≈ 5.0 atol=1e-4
    @test only(sol(5.5e-7, idxs=[sys.node_q])) ≈ 5.0 atol=1e-4
    @test only(sol(7.0e-7, idxs=[sys.node_q])) ≈ 5.0 atol=1e-4
end

using PlotlyLight, Cobweb
# Save out an .html page of the solution, for later analysis
plots_dir = joinpath(Base.pkgdir(CedarSim), "test", "plots")
mkpath(plots_dir)
Cobweb.save(sol, joinpath(plots_dir, "gf180_dff.html"); title="gf180_dff")

using CSV
CSV.write(joinpath(plots_dir, "gf180_dff.csv"), sol)
end
