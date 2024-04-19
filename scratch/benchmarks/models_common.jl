using BenchmarkTools
using CedarSim
using BSIM4
using CedarSim.SpectreEnvironment
using SpectreNetlistParser: SPICENetlistParser
using TimerOutputs
using OrdinaryDiffEq

function load_gf180(;to = TimerOutput(), compute_jacobian::Bool = true)
    repo_root = Base.pkgdir(CedarSim)
    @timeit to "Parsing" begin
        @timeit to "include(BSIM4.bsim4_va)" begin
            if !isdefined(@__MODULE__, :bsim4)
                @eval const bsim4 = load_VA_model(BSIM4.bsim4_va)
            end
        end
        @timeit to "SPICENetlistParser.parsefile" begin
            sa1 = SPICENetlistParser.parsefile(joinpath(
                repo_root,
                "test",
                "DFF",
                "DFF_cap_all.cir",
            ))
        end
        @timeit to "CedarSim.make_spectre_circuit" begin
            code = CedarSim.make_spectre_circuit(sa1, String[repo_root]);
        end
        @timeit to "eval(code)" begin
            circuit = eval(code)
        end
    end
    @timeit to "Compilation" begin
        @timeit to "CircuitIRODESystem(circuit)" begin
            sys = CircuitIRODESystem(circuit; debug_config=(;store_ir_levels=true, store_ss_levels=true))
        end

        @timeit to "DAEProblem(sys)" begin
            daeprob = DAEProblem(sys, nothing, nothing, (0.0, 7e-7); initializealg=CedarDCOp(;abstol=1e-15));
        end

        if compute_jacobian
            @timeit to "DAEProblem(sys; jac=true)" begin
                daeprob_jac = DAEProblem(sys, nothing, nothing, (0.0, 7e-7); initializealg=CedarDCOp(;abstol=1e-15), jac=true);
            end
        else
            daeprob_jac = nothing
        end

        @timeit to "ODEProblem(sys)" begin
            odeprob = ODEProblem(sys, nothing, (0.0, 7e-7); initializealg=CedarDCOp(;abstol=1e-15));
        end

        if compute_jacobian
            @timeit to "ODEProblem(sys; jac=true)" begin
                odeprob_jac = ODEProblem(sys, nothing, (0.0, 7e-7); initializealg=CedarDCOp(;abstol=1e-15), jac=true);
            end
        else
            odeprob_jac = nothing
        end
    end
    return daeprob, daeprob_jac, odeprob, odeprob_jac
end
