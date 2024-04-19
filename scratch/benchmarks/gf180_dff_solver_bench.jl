# We have a `Project.toml` here that provides `BenchmarkTools` and `TimerOutputs`
# This allows us to still load `CedarSim` by pushing its environment onto the
# front of the loading stack, without needing to duplicate that entire project here.
insert!(Base.LOAD_PATH, 1, dirname(dirname(@__DIR__)))

println("--- gf180_dff solver benchmark")

include("benchmark_common.jl")

to = TimerOutput()
daeprob, daeprob_jac, odeprob, odeprob_jac = load_gf180(;to)

using OrdinaryDiffEq
using OrdinaryDiffEq.LineSearches
using Sundials
using Test

solver_configurations = [
    # DFBDF failes with DTLessThanMin
    #daeprob     => ("DAE - DFBDF (linesearch)", DFBDF(;autodiff=false, nlsolve=NLNewton(relax=BackTracking()))),
    
    daeprob     => ("DAE - IDA", IDA()),
    daeprob_jac => ("DAE - IDA (jac)", IDA()),

    # ODEs without jacobians lack precision, so we only use `odeprob_jac`
    # FBDF fails with jacobians though
    #odeprob_jac => ("ODE - FBDF (linesearch, jac)", FBDF(;nlsolve=NLNewton(relax=BackTracking()))),
    
    odeprob_jac => ("ODE - Rodas5P (jac)", Rodas5P()),
    odeprob_jac => ("ODE - Rosenbrock23 (jac)", Rosenbrock23()),
]
const abstol = 1e-4

@timeit to "First Runtimes" begin
    for (prob, (solver_name, solver)) in solver_configurations
        local sol
        @timeit to "short solve()" begin
            @timeit to "$(solver_name)" begin
                try
                    # Shorten the problem to make this a quicker test
                    prob = remake(prob, tspan=(0.0, 1e-8))
                    sol = solve(prob, solver; abstol)
                    @assert sol.retcode == ReturnCode.Success
                catch e
                    if isa(e, InterruptException)
                        rethrow(e)
                    end
                    @error("solve() Failure: benchmarks unreliable!", name=solver_name, abstol, e)
                end
            end
        end
    end
end
display(to)

# For each benchmark configuration, track solve rate
benchmark_sols = Dict{String,Vector}()
benchmark_results = Dict{String,Any}()

# Next, benchmark `solve()`
for (prob, (solver_name, solver)) in solver_configurations
    benchmark_sols[solver_name] = []
    @info("Benchmarking", solver_name)

    benchmark_results[solver_name] = @benchmark begin
        try
            sol = solve(prob, solver; abstol, reltol)
            push!(Main.benchmark_sols[solver_name], sol)
        catch e
            if isa(e, InterruptException)
                rethrow(e)
            end
        end
    end setup=begin
        # Unpack other values so we don't have to be interpolating them all over
        solver = $(solver)
        solver_name = $(solver_name)
        abstol = $(abstol)
        reltol = $(abstol)

        # For setup, randomize initial conditions, then run `init()` to get better benchmark
        # statistics, in the event that there are multiple possible initialization points
        # for the circuit.  But this is disabled for now, as this can take a lot of time.
        #prob = CedarSim.reinitialize($(prob))
        prob = $(prob)
    end seconds=30 evals=1 samples=10

    # Check that the solves were a success
    for (sol_idx, sol) in enumerate(benchmark_sols[solver_name])
        if sol.retcode != ReturnCode.Success
            @error("solve() Failure: benchmarks unreliable!", name=solver_name, retcode=sol.retcode, abstol)
        else
            sys = get_sys(sol)
            for (t, q) in ((1.5e-7, 0.0), (2.5e-7, 0.0), (4.5e-7, 5.0), (5.5e-7, 5.0), (7.0e-7, 5.0))
                if !isapprox(only(sol(t, idxs=[sys.node_q])), q; atol=abstol*10)
                    @error("Solution mismatch", t, q, sol(t, idxs=[sys.node_q]))
                    benchmark_sols[solver_name][sol_idx] = SciMLBase.solution_new_retcode(sol, ReturnCode.Failure)
                end
            end
        end
    end
end

at_least_one_solve_pass = false
# Only show benchmarks for runs that succeeded in all cases:
for (prob, (solver_name, solver)) in solver_configurations
    println("+++ solve() $(solver_name) results")
    sols = benchmark_sols[solver_name]
    if isempty(sols)
        @warn("solve() $(solver_name) no successful solves")
        continue
    end
    solve_rate = sum(sol.retcode == ReturnCode.Success for sol in sols)./length(sols)
    if solve_rate > 0.8
        if solve_rate != 1.0
            @warn("solve() $(solver_name) benchmark SUSPECT due to failures", solve_rate)
        end
        @info("solve() $(solver_name) benchmark:", num_states = length(sols[1].u[1]))
        display(benchmark_results[solver_name])
        display(sols[1].stats)
        global at_least_one_solve_pass = true
    else
        @warn("solve() $(solver_name) benchmark SKIPPED due to too many failures", solve_rate)
    end
    println()
    println("_"^100)
    println()
end

if !at_least_one_solve_pass
    error("We never actually solved gf180_dff with 100% pass rate!")
end
