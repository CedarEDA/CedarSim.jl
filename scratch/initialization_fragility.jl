using CedarSim, OrdinaryDiffEq, NonlinearSolve, SciMLNLSolve, LinearAlgebra, DataFrames

include(joinpath(Base.pkgdir(CedarSim), "test", "common.jl"))

# This file is intended to be used once you already have a `DAEProblem` or `ODEProblem`.
# Example:
#
#     include("test/gf180_dff.jl")
#     include("scratch/initialization_fragility.jl")
#     results = initialization_fragility(gf180.prob)

function reinitialize(prob; abstol=deftol, reltol=deftol)
    if prob isa ODEProblem
        prob = remake(prob, u0=1e-7randn(length(prob.u0)))
    else
        prob = remake(prob, u0=1e-7randn(length(prob.u0)), du0=1e-7randn(length(prob.u0)))
    end
    return prob
end

function initialization_norm(prob, u0)
    tmp = similar(prob.u0)
    if prob isa ODEProblem
        prob.f(tmp, u0, prob.p, 0.)
    else
        prob.f(tmp, u0*0, u0, prob.p, 0.)
    end
    # Note, we're assuming the 2-norm here
    return norm(tmp)
end

function reinitialize_and_norm(prob; nlsolve = NLSolveJL(;store_trace = true, extended_trace=true), abstol=deftol, reltol=deftol)
    tmp = similar(prob.u0)
    local integ
    if prob isa ODEProblem
        prob = remake(prob, u0=1e-7randn(length(prob.u0)))
        integ = init(prob, FBDF(autodiff=false), initializealg=CedarDCOp(;nlsolve, abstol); abstol, reltol)
        prob.f(tmp, integ.u, prob.p, 0.)
    else
        prob = remake(prob, u0=1e-7randn(length(prob.u0)), du0=1e-7randn(length(prob.u0)))
        integ = init(prob, DFBDF(autodiff=false), initializealg=CedarDCOp(;nlsolve, abstol); abstol, reltol)
        prob.f(tmp, integ.du, integ.u, prob.p, 0.)
    end
    norm = integ.opts.internalnorm(tmp, 0.)
    return norm, integ
end

"""
    initialization_fragility(prob, N = 100; kwargs...)

Run reinitialization `N` times on problem `prob`, returning the resulting
statistics in a `DataFrame` for analysis.
"""
function initialization_fragility(prob, N=100; nlsolve=NLSolveJL(), abstol=deftol, reltol=deftol)
    results = DataFrame()
    for _ in 1:N
        # Default case is failure
        loss = missing
        retcode = ReturnCode.Failure
        try
            loss, retcode = reinitialize_and_norm(prob; nlsolve, abstol, reltol)
        catch e
            if !(e isa LinearAlgebra.SingularException)
                rethrow(e)
            end
        end
        push!(results, (; loss = loss, retcode = retcode))
    end
    return results
end
