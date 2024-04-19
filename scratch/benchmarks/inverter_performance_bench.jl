# We have a `Project.toml` here that provides `BenchmarkTools` and `TimerOutputs`
# This allows us to still load `CedarSim` by pushing its environment onto the
# front of the loading stack, without needing to duplicate that entire project here.
insert!(Base.LOAD_PATH, 1, dirname(dirname(@__DIR__)))

println("--- inverter performance benchmark")

include("benchmark_common.jl")

to = TimerOutput()
daeprob, daeprob_jac, odeprob, odeprob_jac = load_inverter(;to)
display(to)

# Do all of the benchmarking
benchmark_prob(daeprob_jac)
benchmark_prob(odeprob_jac)
