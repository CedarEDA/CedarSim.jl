using DescriptorSystems
using OrdinaryDiffEq
using LinearAlgebra
import DAECompiler: get_transformed_sys, IRODESystem

export ac!, acdec, freqresp, noise!

abstract type FreqSol end

struct ACSol <: FreqSol
    dss::DescriptorStateSpace{Float64, Matrix{Float64}}
    sol::SciMLBase.AbstractODESolution
end

struct NoiseSol <: FreqSol
    dss::DescriptorStateSpace{Float64, Matrix{Float64}}
    pwr::Vector{Float64}
    exp::Vector{Float64}
    sol::SciMLBase.AbstractODESolution
end

# Convenience function to get our tsys from an `ACSol` object
# this will also make `DAECompiler.get_sys` work to get the IRODESystem
DAECompiler.get_transformed_sys(ac::ACSol) = ac.sol.prob.f.sys
DAECompiler.get_transformed_sys(ac::NoiseSol) = ac.sol.prob.f.sys



function determine_tangents(@nospecialize(T))
    res = []
    for (name, type) in zip(fieldnames(T), fieldtypes(T))
        if type <: Number
            push!(res, name)
        else
            tup = determine_tangents(type)
            if !isempty(tup)
                push!(res, (name => tup))
            end
        end
    end
    res
end

function check_prob_for_ac(prob)
    if !isa(prob.p, ParamSim)
        throw(ArgumentError("Solutions must be `ParamSim`!"))
    end
    if prob.p.mode != :ac
        throw(ArgumentError("Solutions must be compiled with `mode == :ac`!"))
    end
    if prob.f.jac === nothing || prob.f.paramjac === nothing
        throw(ArgumentError("Solutions must be compiled with `jac = true` and `paramjac = true`!"))
    end
end

function check_prob_for_noise(prob)
    if !isa(prob.p, ParamSim)
        throw(ArgumentError("Solutions must be `ParamSim`!"))
    end
    if prob.p.mode != :ac
        throw(ArgumentError("Solutions must be compiled with `mode == :ac`!"))
    end
    if prob.f.jac === nothing
        throw(ArgumentError("Solutions must be compiled with `jac = true`"))
    end
end

"""
    ACSol(sol::SciMLBase.AbstractODESolution)

Given a DC analysis result (which must be parameterized on `ϵω` and use `:ac`
mode), construct an `ACSol` object representing the linearization of the
circuit around that DC operating point.  Also see `ac!()`.
"""
function ACSol(sol::SciMLBase.AbstractODESolution)
    prob = sol.prob
    check_prob_for_ac(prob)
    if !hasproperty(prob.p.spec, :ϵω)
        throw(ArgumentError("AC Solutions must be parameterized on `ϵω`!"))
    end

    M = prob.f.mass_matrix
    num_params = DAECompiler.determine_num_tangents(DAECompiler.parameter_type(get_sys(sol)))
    num_eqs = length(prob.u0)

    Jp = zeros(num_eqs, num_params)
    prob.f.paramjac(Jp, sol.u[1], prob.p, 0.0)
    Ju = zeros(num_eqs, num_eqs)
    prob.f.jac(Ju, sol.u[1], prob.p, 0.0)

    # Note that in control systems x is the state and u is the input
    # Edx = Ax + Bu
    #   y = Cx + Du
    # C*((w[i]*E - A)^-1)*B + D

    acidx = findfirst(==(:ϵω), propertynames(prob.p.spec))
    B = Jp[:, acidx]
    C = I(num_eqs)
    dsys = dss(Ju, M, B, C, 0)

    return ACSol(dsys, sol)
end

function noise_exp(sol, sys)
    T = Union{Nothing, Float64, DAECompiler.ScopeRef}
    result = getfield(sys, :result)
    pwr = Vector{T}(undef, result.neps)
    exp = Vector{T}(undef, result.neps)
    noise_exp(pwr, exp, result.names, sys)
    @assert length(pwr) == length(exp)
    pwridxs = isa.(pwr, DAECompiler.ScopeRef)
    expidxs = isa.(exp, DAECompiler.ScopeRef)
    pwrrefs = convert(Vector{DAECompiler.ScopeRef}, pwr[pwridxs])
    exprefs = convert(Vector{DAECompiler.ScopeRef}, exp[expidxs])
    obs = vcat(pwrrefs, exprefs)
    rec = DAECompiler.batch_reconstruct(sol, obs)
    pwr[pwridxs] = rec[begin:length(pwrrefs)]
    exp[expidxs] = rec[length(pwrrefs)+1:end]
    return convert(Vector{Float64}, pwr), convert(Vector{Float64}, exp)
end
function noise_exp(pwr, exp, names, scope)
    for (k, v) in pairs(names)
        if v.children !== nothing
            noise_exp(pwr, exp, v.children, getproperty(scope, k))
        end
        v.eps !== nothing || continue
        expk = Symbol(k, :exp)
        pwrk = Symbol(k, :pwr)
        @assert hasproperty(scope, pwrk)
        pwr[v.eps] = getproperty(scope, pwrk)
        if hasproperty(scope, expk)
            exp[v.eps] = getproperty(scope, expk)
        else
            exp[v.eps] = 0.
        end
    end
end

function NoiseSol(sol::SciMLBase.AbstractODESolution)
    prob = sol.prob
    check_prob_for_noise(prob)

    M = prob.f.mass_matrix
    tsys = DAECompiler.get_transformed_sys(sol)
    num_epsilons = tsys.state.neps
    num_eqs = length(prob.u0)

    Jε = zeros(num_eqs, num_epsilons)
    epsjac! = get_epsjac(prob)
    epsjac!(Jε, sol.u[1], prob.p, 0.0)
    Ju = zeros(num_eqs, num_eqs)
    prob.f.jac(Ju, sol.u[1], prob.p, 0.0)

    # Note that in control systems x is the state and u is the input
    # Edx = Ax + Bu
    #   y = Cx + Du
    # C*((w[i]*E - A)^-1)*B + D

    C = I(num_eqs)
    dsys = dss(Ju, M, Jε, C, 0)

    pwr, exp = noise_exp(sol, get_sys(sol))

    return NoiseSol(dsys, pwr, exp, sol)
end

"""
    ac!(circ; debug_config = (;), kwargs...)

Construct an `ACSol` object for the given circuit, allowing solution of
AC analysis for observables within the circuit.
"""
function ac!(circ; debug_config = (;), u0=nothing, kwargs...)
    # Parameterize circ on `ϵω` and set `mode` to `:ac`
    circ = CedarSim.ParamSim(circ; mode=:ac, ϵω=0.0)
    sys = CircuitIRODESystem(circ; debug_config)
    prob = ODEProblem(sys, u0, (0.0, 1.0), circ, jac=true, paramjac=true, initializealg=CedarDCOp())
    integ = init(prob, FBDF(autodiff=false); kwargs...)
    return ACSol(integ.sol)
end

function noise!(circ; debug_config = (;), u0=nothing, kwargs...)
    # Parameterize circ on all noise parameters and set `mode` to `:ac`
    np = noiseparams(circ)
    circ = CedarSim.ParamSim(circ; mode=:ac)
    sys = CircuitIRODESystem(circ; debug_config)
    prob = ODEProblem(sys, u0, (0.0, 1.0), circ, jac=true, initializealg=CedarDCOp())
    integ = init(prob, FBDF(autodiff=false); kwargs...)
    return NoiseSol(integ.sol)
end

"""
    acjac(ac::ACSol, syms)

Given a list of `syms`, calculate the linearized AC sensitivities (e.g. the
derivative about that operating point) for those variables/observables.
"""
function acjac(ac::FreqSol, syms)
    var_inds, obs_inds = DAECompiler.split_and_sort_syms(syms)
    transformed_sys = DAECompiler.get_transformed_sys(ac)
    dreconstruct! = get!(ac.sol.prob.f.observed.derivative_cache, (var_inds, obs_inds, true)) do
        DAECompiler.compile_batched_reconstruct_derivatives(transformed_sys, var_inds, obs_inds, true, false;)
    end
    u = ac.sol.u[1]
    num_params = DAECompiler.determine_num_tangents(typeof(ac.sol.prob.p))
    neps = getfield(transformed_sys.state.sys, :result).neps
    dvars_du = similar(u, length(var_inds), length(u))
    dvars_dp = similar(u, length(var_inds), num_params)
    dvars_deps = similar(u, length(var_inds), neps)
    dobs_du = similar(u, length(obs_inds), length(u))
    dobs_dp = similar(u, length(obs_inds), num_params)
    dobs_deps = similar(u, length(obs_inds), neps)
    dreconstruct!(dvars_du, dvars_dp, dvars_deps, dobs_du, dobs_dp, dobs_deps, u, ac.sol.prob.p, 0.0)

    return dvars_du, dvars_dp, dvars_deps, dobs_du, dobs_dp, dobs_deps
end

"""
    getindex(ac::ACSol, ref::ScopeRef)

Calculate the descriptor state-space model of the given AC Solution with
respect to the indicated `ScopeRef`.
"""
function Base.getindex(ac::NoiseSol, ref::DAECompiler.ScopeRef)
    # We `vcat` these, knowing that there will be only one return value
    # as we only allow a single `sym` through at once.  We call that `dvarobs_du`
    # as we're not making a distinction between variables and observables here.
    dvars_du, dvars_dp, dvars_deps, dobs_du, dobs_dp, dobs_deps = acjac(ac, [ref])
    dvarobs_du = vcat(dvars_du, dobs_du)
    dvarobs_dp = vcat(dvars_dp, dobs_dp)
    dvarobs_deps = vcat(dvars_deps, dobs_deps)

    # Construct DescriptorStateSpace object that we can then sample at the given frequencies
    A, E, B, C, D = dssdata(ac.dss)
    return dss(A, E, B, dvarobs_du, dvarobs_deps)
end

function Base.getindex(ac::ACSol, ref::DAECompiler.ScopeRef)
    # We `vcat` these, knowing that there will be only one return value
    # as we only allow a single `sym` through at once.  We call that `dvarobs_du`
    # as we're not making a distinction between variables and observables here.
    dvars_du, dvars_dp, _, dobs_du, dobs_dp, _ = acjac(ac, [ref])
    dvarobs_du = vcat(dvars_du, dobs_du)
    dvarobs_dp = vcat(dvars_dp, dobs_dp)

    # Construct DescriptorStateSpace object that we can then sample at the given frequencies
    A, E, B, C, D = dssdata(ac.dss)
    prob = ac.sol.prob
    paths = VectorPrisms.paths(typeof(prob.p))
    idxs = findall(contains('ϵ'), paths)
    return dss(A, E, B, dvarobs_du, dvarobs_dp[:, idxs])
end


"""
    freqresp(sol::ACSol, sym::ScopeRef, ωs::Vector{Float64})

Calculate the frequency response of the given AC Solution for `sym` across the
frequencies listed in `ωs`.  Returns a `Vector{ComplexF64}`.
"""
function DescriptorSystems.freqresp(ac::ACSol,
                                    sym::DAECompiler.ScopeRef,
                                    ωs::Vector{Float64})
    return vec(DescriptorSystems.freqresp(ac[sym], ωs))
end

function PSD(dss::DescriptorStateSpace, ωs::Vector{Float64}, pwr, exp)
    fr = DescriptorSystems.freqresp(dss, ωs)
    out = similar(fr, eltype(fr), (size(fr, 1), size(fr, 1), length(ωs)))
    # out = similar(fr)
    for (i, ω) in enumerate(ωs)
        f = ω/(2π)
        S = Diagonal(@. pwr/f^exp)
        out[:, :, i] = fr[:, :, i] * S * fr[:, :, i]'
    end
    vec(out)
end
function PSD(ac::NoiseSol,
             sym::DAECompiler.ScopeRef,
             ωs::Vector{Float64})
    # sysbal, D1, D1 = gprescale(ac[sym])
    sysbal = ac[sym]
    PSD(sysbal, ωs, ac.pwr, ac.exp)
end

"""
    acdec(nd, fstart, fstop)

Generate a logarithmically spaced frequency vector from `fstart` to `fstop`
with `nd` points per decade.  Equivalent to the SPICE command:

    .ac dec nd fstart fstop

Return value is a vector in hertz per second.
"""
function acdec(nd, fstart, fstop)
    fstart = log10(fstart)
    fstop = log10(fstop)
    points = Int(ceil((fstop-fstart)*nd))+1
    return exp10.(range(fstart, stop=fstop, length=points))
end
