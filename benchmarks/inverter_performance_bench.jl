println("--- inverter performance benchmark")

include("benchmark_common.jl")

to = TimerOutput()
daeprob, daeprob_jac, odeprob, odeprob_jac = load_inverter(;to)
display(to)

# Do all of the benchmarking
benchmark_prob(daeprob_jac)
benchmark_prob(odeprob_jac)
