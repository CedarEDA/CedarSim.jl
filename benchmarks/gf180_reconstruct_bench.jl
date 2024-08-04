println("--- gf180_dff reconstruction benchmark")

include("benchmark_common.jl")
using CedarSim: DAECompiler
to = TimerOutput()
daeprob, _, odeprob, _ = load_gf180(;to, compute_jacobian = false)

function compile_reconstruct(prob; vars = Int64[], obs = Int64[])
    t_compile = @elapsed begin
        reconstruct = DAECompiler.compile_batched_reconstruct_func(prob.f.sys, vars, obs, isa(prob, DAEProblem))
    end
    out_vars = similar(prob.u0, length(vars))
    out_obs = similar(prob.u0, length(obs))
    if isa(prob, DAEProblem)
        du = zero(prob.u0)
        func = () -> begin
            reconstruct(out_vars, out_obs, du, prob.u0, prob.p, 0.0)
        end
    else
        func = () -> begin
            reconstruct(out_vars, out_obs, prob.u0, prob.p, 0.0)
        end
    end
    return t_compile, func
end

# We test a variety of cases; first, the `"1 var"` case is basically testing the
# ability of the compiler to drop every IR statement in the IR code, since a
# purely-`var` reconstruction should compile down to basically just `u_out[idx] = u_compressed[var_idx]`.
# So the comparison of `1 var` to `1 obs` shows us how well the compiler is dropping
# unrelated IR statements.
# The actual reconstruction benchmark we're interested in is to pull out `node_d` and `node_q`
benchmark_sets = [
    ("DAE - 1 var", daeprob, Int64[1], Int64[]),
    ("DAE - 1 obs", daeprob, Int64[],  Int64[1]),
    ("DAE - Q/D",   daeprob, DAECompiler.split_and_sort_syms([get_sys(daeprob).node_q, get_sys(daeprob).node_d])...),

    ("ODE - 1 var", odeprob, Int64[1], Int64[]),
    ("ODE - 1 obs", odeprob, Int64[],  Int64[1]),
    ("ODE - Q/D",   odeprob, DAECompiler.split_and_sort_syms([get_sys(odeprob).node_q, get_sys(odeprob).node_d])...),
]


for (name, prob, vars, obs) in benchmark_sets
    t_compile, func = compile_reconstruct(prob; vars, obs);
    t_runtime = @benchmark $func()
    println("--- $(name)")
    @info(name, t_compile)
    display(t_runtime)
    println()
end
