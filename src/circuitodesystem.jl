using DiffEqBase
import Random

export CircuitIRODESystem, CircuitDynODESystem, DefaultSim, ParamSim

abstract type AbstractSim{C} <: AbstractRecordVector{Float64} end

function DAECompiler.default_parameters(::Type{T}) where {C, T<:AbstractSim{C}}
    return T(DAECompiler.default_parameters(C))
end

struct DefaultSim{T} <: AbstractSim{T}
    circuit::T
    mode::Symbol
end
DefaultSim(circuit) = DefaultSim(circuit, :tran)
DefaultSim{C}(circuit::C) where {C} = DefaultSim(circuit, :tran)


function (sim::DefaultSim)()
    circuit′ = getfield(sim, :circuit)
    with(spec=>SimSpec(time=DAECompiler.sim_time()), sim_mode=>sim.mode, debug_scope=>DScope()) do
        return circuit′()
    end
end

floatparams(p) = float(p)
floatparams(::Nothing) = nothing
function floatparams(p::NamedTuple)
    return NamedTuple((k => floatparams(v) for (k, v) in pairs(p)))
end

function splitparams(params)
    names = keys(params)
    spec_names = fieldnames(SimSpec) ∩ names
    circ_names = setdiff(names, spec_names)
    return (tuple(((p, getfield(params, p)) for p in spec_names)...),
            tuple(((p, getfield(params, p)) for p in circ_names)...))
end

"""
    ParamSim(circuit; params...)

A simulation that uses the given circuit and parameters.
These can be both circuit parameters and `SimSpec` parameters.

The circuit can be either a struct type or a function that takes a `ParamLens`.

In the case of a function, it is called with a `ParamLens` containting the parameters
as its first argument.  This is compatible with functions generated from SPICE or
Spectre netlists.

In the struct case, the lens is applied recursively to the fields of the struct,
originally constructed from the struct's default constructor, i.e. it is equivalent
to `ParamSim(🔍->(🔍(T()))())`.

This is because DAECompiler has to assume that any simulation parameter can be changed.
Passing a circuit with many parameters to `DefaultSim` will produce less efficient code,
because it can't inline any of the parameters as constant.

With `ParamSim`, only the parameters that are passed to the constructor can be changed.
Any other circuit parameters will be inlined as constants.

For a function with no parameters or a struct without a keyword constructor, use `DefaultSim`.
"""
struct ParamSim{T, S, P} <: AbstractSim{T}
    circuit::T
    mode::Symbol # mode is used in initialisation, so it is required
    spec::S
    params::P

    # Always use `Core.Typeof()` for extra type-inferrence _magic_
    # This is necessary so that `Accessors.set()` maintains the `T == Type{Foo}`
    function ParamSim(circuit, mode, spec, params)
        return new{Core.Typeof(circuit),typeof(spec),typeof(params)}(circuit, mode, spec, params)
    end
end

function ParamSim(circuit; mode=:tran, params...)
    spec, cirparams = splitparams(floatparams(values(params)))
    cirparams = nest_param_list(cirparams)
    spec = nest_param_list(spec)
    if !isa(circuit, Type)
        cirparams = canonicalize_params(cirparams)
    end
    return ParamSim(circuit, mode, spec, cirparams)
end
ParamSim(ps::ParamSim; kwargs...) = ParamSim(ps.circuit; mode=ps.mode, ps.spec..., ps.params.params..., kwargs...)

# If our `circuit` type is a function, we pass in a `ParamLens` of the
# parameters as a first argument.
function (sim::ParamSim)()
    new_spec = SimSpec(;time=DAECompiler.sim_time(), sim.spec...)
    return with(spec => new_spec, sim_mode => sim.mode, debug_scope=>DScope()) do
        sim.circuit(ParamLens(sim.params))
    end
end

# We store lists of nested symbol selectors and values like `((Symbol("x1.x2.x3.r")), 1), ...)`
# but we might need to turn them into nested named tuples, like `(x1=(x2=(x3=(r=1))), ...)`.
@noinline function nest_param_list(x)
    # collect all keys that nest by the same prefix:
    keys_by_prefix = Dict{Symbol,Vector{Tuple{Symbol,Symbol,Any}}}()
    without_prefix = Dict{Symbol,Any}()
    for (k, v) in x
        kstr = string(k)
        idx = findfirst('.', kstr)
        v === nothing && (v = (;))
        if idx !== nothing
            # If this key has a prefix, store it with its siblings
            prefix = Symbol(kstr[1:idx-1])
            tail = Symbol(kstr[idx+1:end])
            if !haskey(keys_by_prefix, prefix)
                keys_by_prefix[prefix] = Tuple{Symbol,Symbol,Any}[]
            end
            push!(keys_by_prefix[prefix], (Symbol(kstr), tail, v))
        else
            if haskey(without_prefix, Symbol(kstr))
                cedarthrow(ArgumentError("Cannot double-assign a key: $(kstr)"))
            end
            without_prefix[Symbol(kstr)] = v
        end
    end

    clobbered_prefixes = intersect(keys(keys_by_prefix), keys(without_prefix))
    if !isempty(clobbered_prefixes)
        cedarthrow(ArgumentError("Cannot assign to a prefix of another key: $(clobbered_prefixes)"))
    end

    # Recursively collapse each `keys_by_prefix` mapping into a Dict
    collapsed = Dict(prefix => nest_param_list(Dict(tail => v for (k, tail, v) in subs)) for (prefix, subs) in keys_by_prefix)
    return NamedTuple(merge(collapsed, without_prefix))
end

@noinline function flatten_param_list(x, prefix="", syms = Tuple{Symbol,Any}[])
    prefix_str(prefix, k) = isempty(prefix) ? string(k) : string(prefix, ".", k)
    for (k, v) in pairs(x)
        if isa(v, NamedTuple)
            flatten_param_list(v, prefix_str(prefix, k), syms)
        else
            push!(syms, (Symbol(prefix_str(prefix, k)), v))
        end
    end
    return tuple(sort(syms, by = ((k, v),) -> k)...)
end

function CircuitIRODESystem(circuit::AbstractSim; kwargs...)
    tt = Tuple{typeof(circuit)}
    return IRODESystem(tt; kwargs...)
end

function CircuitIRODESystem(circuit; kwargs...)
    tt = Tuple{typeof(DefaultSim(circuit))}
    return IRODESystem(tt; kwargs...)
end

function CircuitIRODESystem(circT::Type{<:AbstractVector{Float64}}; kwargs...)
    tt = Tuple{DefaultSim{circT}}
    return IRODESystem(tt; kwargs...)
end
function CircuitIRODESystem(circT::Type; kwargs...)
    tt = Tuple{DefaultSim{VectorPrism{Float64, circT}}}
    return IRODESystem(tt; kwargs...)
end

