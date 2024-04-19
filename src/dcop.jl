using OrdinaryDiffEq
using OrdinaryDiffEq: get_tmp_cache, recursivecopy!, alg_extrapolates, default_nlsolve
using OrdinaryDiffEq.LineSearches
using LinearAlgebra
using NonlinearSolve
using SciMLBase
using Accessors
using ADTypes
using Sundials
using SciMLBase: NLStats

export CedarDCOp, CedarTranOp

"""
    struct CedarDCOp

Initializes the DAE by
    1. Switching the circuit to :dcop mode, which sets parameters to their dc
       values and turns off time dependence.
    2. Solving the dc steady state problem
    3. Switching the circuit back to its original mode, redoing initalization
       using the dcop as the initialization. By default this is BrownFullBasicInit.
"""
struct CedarDCOp{NLSOLVE} <: DiffEqBase.DAEInitializationAlgorithm
    abstol::Float64
    nlsolve::NLSOLVE
end
CedarDCOp(;abstol=1e-10, nlsolve=RobustMultiNewton(autodiff=AutoFiniteDiff())) = CedarDCOp(abstol, nlsolve)

"""
    struct CedarTranOp

Initializes the DAE by
    1. Switching the circuit to :tranop mode, which turns off time dependence.
    2. Solving the steady state problem to initialize differential vars
    3. Switching the circuit back to its original mode, redoing initalization
       using the tranop as the initialization. By default this is BrownFullBasicInit.
"""
struct CedarTranOp{NLSOLVE} <: DiffEqBase.DAEInitializationAlgorithm
    abstol::Float64
    nlsolve::NLSOLVE
end
CedarTranOp(;abstol=1e-10, nlsolve=RobustMultiNewton(autodiff=AutoFiniteDiff())) = CedarTranOp(abstol, nlsolve)

"""
    bootstrapped_nlsolve(nlprob, nlsolve, abstol)

This attempts to defeat local minima traps when performing our initial
nonlinear solve by starting from multiple initial points (by default 10) and
iteratively solving each separate trajectory until one satisfies the given
tolerance.
"""
function bootstrapped_nlsolve(nlprob, nlsolve, abstol; num_trajectories=10, maxiters=200)
    stats = NLStats(0,0,0,0,0)
    last_residual = nothing
    local prob
    # TODO: Use an ensemble problem for this to initialize in parallel
    for idx in 1:num_trajectories
        try
            prob = remake(nlprob, u0=1e-7randn(size(nlprob.u0)))
            sol = solve(prob, nlsolve; abstol, maxiters)
            last_residual = sol.resid
            stats.nf += sol.stats.nf
            stats.njacs += sol.stats.njacs
            stats.nfactors += sol.stats.nfactors
            stats.nsolve += sol.stats.nsolve
            stats.nsteps += sol.stats.nsteps
            prob.u0 .= sol.u
            if sol.retcode == ReturnCode.Success
                return SciMLBase.build_solution(prob, nlsolve, sol.u, sol.resid; retcode = sol.retcode,
                    original = sol.original, stats = stats)
            end
        catch e
            # Sometimes `solve()` tells us it's singular, we'll just try again
            if isa(e, LinearAlgebra.SingularException)
                prob.u0 .= 1e-7randn(size(prob.u0))
                continue
            end
            # In all other cases, `rethrow()`
            rethrow(e)
        end
    end

    # We were unable to initialize, return a `MaxIters` failure object
    return SciMLBase.build_solution(
        prob,
        nlsolve,
        prob.u0,
        last_residual;
        retcode = ReturnCode.MaxIters,
        original = nothing,
        stats = stats,
    )
end

function OrdinaryDiffEq._initialize_dae!(integrator, prob::Union{ODEProblem, DAEProblem},
                                         alg::Union{CedarDCOp, CedarTranOp}, ::Val{true})
    (; abstol, internalnorm) = integrator.opts
    # We often  want to control the initialization problem's `abstol` separately.
    # The final BrownFullBasic init will also read from `alg.abstol`
    abstol = min(abstol, alg.abstol)
    # TODO: delete this when OrdinaryDiffEq fixes it's initialize call to call BrownFullBasicInit
    if integrator.t == prob.tspan[1]
    # Step 1: Do DC initialization
    (;p, f) = prob
    u0 = integrator.u
    t = integrator.t
    tmp = get_tmp_cache(integrator)[1]
    if prob isa DAEProblem
        du0 = integrator.du
        du0 .= 0.
        f(tmp, du0, u0, p, 0.)
        nlf! = (out, u, p)->f(out, du0, u, p, t)
    else
        f(tmp, u0, p, 0.)
        nlf! = (out, u, p)->f(out, u, p, t)
    end
    integrator.stats.nf += 1
    if internalnorm(tmp, 0.) < abstol
        return
    end
    # TODO: Cedar specific homotopies, etc.
    nlprob = NonlinearProblem(nlf!, u0)

    nl_p = if alg isa CedarDCOp
        Accessors.set(p, (@optic _.mode), :dcop)
    else
        Accessors.set(p, (@optic _.mode), :tranop)
    end
    nlprob = NonlinearProblem(nlf!, u0, nl_p)

    # Solve nonlinear problem to given abstol
    sol = bootstrapped_nlsolve(nlprob, alg.nlsolve, abstol)
    stats = sol.stats
    integrator.stats.nf += stats.nf
    integrator.stats.nsolve += stats.nsolve
    integrator.stats.njacs += stats.njacs
    integrator.stats.nnonliniter += stats.nsteps
    integrator.stats.nnonlinconvfail += sol.retcode != ReturnCode.Success
    integrator.u .= sol.u
    if sol.retcode != ReturnCode.Success
        @warn "DC operating point analysis failed. Further failures may follow."
        integrator.sol = SciMLBase.solution_new_retcode(integrator.sol,
                                                        ReturnCode.InitialFailure)
    end

    recursivecopy!(integrator.uprev, integrator.u)
    if alg_extrapolates(integrator.alg)
        recursivecopy!(integrator.uprev2, integrator.uprev)
    end
    end
    # Step 2: Fix the differential vars and do regular BrownFullBasicInit
    OrdinaryDiffEq._initialize_dae!(integrator, prob, BrownFullBasicInit(;abstol, alg.nlsolve), Val{true}())
    return
end

function SciMLBase.initialize_dae!(integrator::Sundials.IDAIntegrator, alg::Union{CedarDCOp, CedarTranOp})
    prob = integrator.sol.prob
    # We often  want to control the initialization problem's `abstol` separately.
    # The final BrownFullBasic init will also read from `alg.abstol`
    abstol = min(integrator.opts.abstol, alg.abstol)

    # Step 1: Do DC initialization
    (;p, f) = prob
    u0 = integrator.u
    du0 = integrator.du
    t = integrator.t
    tmp = similar(u0)
    #only DAE supported for IDA
    du0 .= 0.
    f(tmp, du0, u0, p, 0.)
    if norm(tmp, 0.) < abstol
        return
    end
    nlf! = (out, u, p)->f(out, du0, u, p, t)
    # TODO: Cedar specific homotopies, etc.
    nlprob = NonlinearProblem(nlf!, u0)

    nl_p = if alg isa CedarDCOp
        Accessors.set(p, (@optic _.mode), :dcop)
    else
        Accessors.set(p, (@optic _.mode), :tranop)
    end
    nlprob = NonlinearProblem(nlf!, u0, nl_p)

    # Solve nonlinear problem to given abstol
    sol = bootstrapped_nlsolve(nlprob, alg.nlsolve, abstol)
    integrator.u .= sol.u
    if sol.retcode != ReturnCode.Success
        @warn "DC operating point analysis failed. Further failures may follow."
        integrator.sol = SciMLBase.solution_new_retcode(integrator.sol,
                                                        ReturnCode.InitialFailure)
    end

    recursivecopy!(integrator.uprev, integrator.u)
    integrator.u_modified = true
    # Step 2: Fix the differential vars and do regular BrownFullBasicInit
    SciMLBase.initialize_dae!(integrator, Sundials.IDADefaultInit())
    return
end

default_solver(::ODEProblem) =  FBDF(autodiff=AutoFiniteDiff(), nlsolve=RobustMultiNewton(autodiff=AutoFiniteDiff()))
default_solver(::DAEProblem) = DFBDF(autodiff=AutoFiniteDiff(), nlsolve=RobustMultiNewton(autodiff=AutoFiniteDiff()))

"""
    reinitialize(prob; solver = default_solver(prob), alg, abstol)

Reinitialize the given `prob` using a random starting point and using the initialization
algorithm `alg` to attempt to find a starting point that satisfies `abstol`.
"""
function reinitialize(prob::Union{ODEProblem, DAEProblem};
                      solver = default_solver(prob),
                      alg::Union{CedarDCOp, CedarTranOp} = prob.kwargs[:initializealg],
                      abstol=alg.abstol,
                      )
    if isa(prob, ODEProblem)
        prob = remake(prob, u0=1e-7randn(length(prob.u0)))
    else
        prob = remake(prob, u0=1e-7randn(length(prob.u0)), du0=1e-7randn(length(prob.u0)))
    end

    # Initialize, then copy over the `u` and `du` to our problem
    integ = init(prob, solver, initializealg=alg; abstol)
    prob.u0 .= integ.u
    if isa(prob, DAEProblem)
        prob.du0 .= integ.du
    end

    # Return the new problem
    return prob
end

"""
    initialization_norm(prob)

Given an `ODEProblem` or `DAEProblem` in `prob`, evaluate the stored initial conditions
and return a norm (by default, the L2 norm) of the result.  The smaller this value is,
the better the initialization.
"""
function initialization_norm(prob::Union{ODEProblem, DAEProblem}, norm::Function = LinearAlgebra.norm)
    tmp = similar(prob.u0)
    if isa(prob, ODEProblem)
        prob.f(tmp, prob.u0, prob.p, 0.)
    else
        prob.f(tmp, prob.du0, prob.u0, prob.p, 0.)
    end
    return norm(tmp)
end
