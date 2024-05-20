using ForwardDiff
using ForwardDiff: Dual
using DAECompiler
using DAECompiler: variable, equation, equation!
using DAECompiler.Intrinsics: AbstractScope
using Base.ScopedValues
const DScope = DAECompiler.Intrinsics.Scope
using Base.Experimental: @MethodTable, @overlay
import Random
export net, Named

Base.@kwdef struct SimSpec
    time::Float64=0.0
    ϵω::Float64=0.0
    temp::DefaultOr{Float64}=mkdefault(27.0)
    gmin::DefaultOr{Float64}=mkdefault(1e-12)
    scale::DefaultOr{Float64}=mkdefault(1.0)
    # rng currently breaks incidence analysis
    rng::Union{#=Random.Xoshiro,=# Nothing}=nothing
end

const debug_scope = ScopedValue{AbstractScope}(DScope())
const spec = ScopedValue{SimSpec}(SimSpec())
const sim_mode = ScopedValue{Symbol}(:dcop)

abstract type AbstractNet end

struct Net{T} <: AbstractNet
    V::T
    kcl!::equation
    multiplier::Float64
    function Net(name::Name=nothing, multiplier::Float64 = 1.0) where Name<:Union{AbstractScope,Nothing}
        V = variable(name)
        kcl! = equation(name)
        dVdt = ddt(V)
        if multiplier < 0.0
            throw(ArgumentError("Cannot construct a Net with non-positive multiplier '$(multiplier)'"))
        end
        obj = new{typeof(dVdt)}(V, kcl!, multiplier)
        return obj
    end

    function Net(net::Net{T}, multiplier::Float64) where {T}
        multiplier = net.multiplier * multiplier
        if multiplier < 0.0
            throw(ArgumentError("Cannot construct a Net with non-positive multiplier '$(multiplier)'"))
        end
        return new{T}(net.V, net.kcl!, multiplier)
    end
end
Net(name::Symbol) = Net(DScope(DScope(), name))
Net(name::String) = Net(Symbol(name))

kcl!(net::AbstractNet, current) = net.kcl!(net.multiplier * current)

struct ParallelInstances
    device
    multiplier::Float64

    function ParallelInstances(device, multiplier::Float64)
        if multiplier < 0.0
            cedarthrow(ArgumentError("Cannot construct a ParallelInstances with non-positive multiplier '$(multiplier)'"))
        end
        return new(device, multiplier)
    end
end
ParallelInstances(device, multiplier::Number) = ParallelInstances(device, Float64(multiplier))
ParallelInstances(device, multiplier::DefaultOr) = ParallelInstances(device, undefault(multiplier))

Base.@constprop :aggressive function (pi::ParallelInstances)(nets...; kwargs...)
    nets = map(nets) do net
        Net(net, pi.multiplier)
    end
    return pi.device(nets...; kwargs...)
end

net(args...) = Net(args...)

struct Named{T}
    element::T
    name::Symbol
    Named(element::T, name::Union{String, Symbol}) where {T} = new{Core.Typeof(element)}(element, Symbol(name))
end

function (n::Named)(args...)
    return n.element(args...; dscope=DScope(debug_scope[], n.name))
end

function (n::Named{typeof(net)})(args...)
    return Net(DScope(debug_scope[], n.name))
end

"""
    branch!([f,] [scope,] net₊::AbstractNet, net₋::AbstractNet)

Create a branch between the two nets `net₊` and `net₋`. Without a callback
`f`, `branch!` will return (V, I), the voltage difference across the branch
as well as the current through the branch.

Optionally, a callback `f` may be provided, in which case `(V, I)` will be provided
as arguments `f` and `branch!` will call `equation!` on the return value of `f`.
In general, the callback form is preferred, because it ensures that the resulting
system is balanced (in particular, the variable that is introduced for the current
is balanced by returned equation).

Optionally, a scope may be provided. In this case, the scope will be used to namespace
the current variable `:I`, as well as providing an observable `:V` in the same scope
for the voltage across the branch.
"""
function branch! end

function branch!(scope::AbstractScope, net₊::AbstractNet, net₋::AbstractNet)
    # Branch current - semantically flows from net₊ to net₋
    I = variable(scope(:I))
    kcl!(net₊, -I)
    kcl!(net₋,  I)
    V = net₊.V - net₋.V
    observed!(V, scope(:V))
    (V, I)
end
branch!(scope::Symbol, net₊::AbstractNet, net₋::AbstractNet) =
    branch!(defaultscope(scope), net₊, net₋)

function branch!(scope::Nothing, net₊::AbstractNet, net₋::AbstractNet)
    I = variable()
    kcl!(A, -I)
    kcl!(B,  I)
    V = A.V - B.V
    observed!(V)
    (V, I)
end
branch!(net₊::AbstractNet, net₋::AbstractNet) = branch!(nothing, net₊, net₋)

function branch!(f, net₊::AbstractNet, net₋::AbstractNet)
    equation!(f(branch!(net₊, net₋)...))
end

function branch!(f, scope::Union{Nothing, AbstractScope}, net₊::AbstractNet, net₋::AbstractNet)
    equation!(f(branch!(scope, net₊, net₋)...))
end

@inline function DAECompiler.equation!(val::Dual{SimTag, <:Number, 1}, scope)
    DAECompiler.equation!(ForwardDiff.value(SimTag, val), scope)
end

@inline function DAECompiler.equation!(val::Dual{SimTag, <:Number, 1})
    DAECompiler.equation!(ForwardDiff.value(SimTag, val))
end

@inline function DAECompiler.observed!(val::Dual{SimTag, <:Number, 1}, name)
    DAECompiler.observed!(ForwardDiff.value(SimTag, val), name)
end

@inline function DAECompiler.observed!(val::Dual{SimTag, <:Number, 1})
    DAECompiler.observed!(ForwardDiff.value(SimTag, val))
end
