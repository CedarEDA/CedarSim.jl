using ChainRulesCore, StaticArrays

# This gets imported by all generated Spectre code. The function names
# exported here should correspond to what is made available by Spectre

struct PWLConstructError <: CedarException
    ts
    ys
end

function Base.showerror(io::IO, err::PWLConstructError)
    println(io, "PWL must have an equal number of x and y values")
end

function find_t_in_ts(ts, t)
    idx = Base.searchsortedfirst(ts, t)
    if idx <= length(ts) && ts[idx] == t
        return idx + 1
    end
    return idx
end

# Disable autodiff through `t` search
function ChainRulesCore.frule((_, _), ::typeof(find_t_in_ts), ts, t)
    return find_t_in_ts(ts, t), ZeroTangent()
end


rem_right_semi(t, r) = t % r
function ChainRulesCore.frule((_, δt, δr), ::typeof(rem_right_semi), t, r)
    return (rem_right_semi(t, r), δt)
end

# Split our `wave` into `ts` and `ys`, hinting to the compiler what the length
# of these views are.
function wave_split(wave::SVector)
    idxs = SVector{div(length(wave),2)}(1:2:length(wave))
    ts = @view(wave[idxs])
    ys = @view(wave[idxs.+1])
    return (ts, ys)
end

function pwl_at_time(ts, ys, t)
    if length(ts) != length(ys)
        throw(CedarSim.PWLConstructError(ts, ys))
    end
    i = find_t_in_ts(ts, t)
    type_stable_time = 0. * t
    if i <= 1
        # Signal is before the first timepoint, hold the first value.
        return ys[1] + type_stable_time
    end
    if i > length(ts)
        # Signal is beyond the final timepoint, hold the final value.
        return ys[end] + type_stable_time
    end
    if ys[i-1] == ys[i]
        # signal is constant/flat (singularity in y)
        return ys[i] + type_stable_time
    end
    if ts[i] == ts[i-1]
        # signal is infinitely steep (singularity in t)
        # This can occur when loading a serialized signal that had its time digits truncated.
        return (ys[i-1] + ys[i])/2 + type_stable_time
    end
    # The general case, where we must perform linear interpolation
    slope = (ys[i] - ys[i-1])/(ts[i] - ts[i-1])
    return ys[i-1] + (t - ts[i-1])*slope
end

@generated function time_periodic_singularities!(ts::StaticArrays.SVector, period = ts[end], count = 1)
    body = Expr(:block)
    for i in 1:length(ts) # length of the type!
        push!(body.args, :(DAECompiler.time_periodic_singularity!(ts[$i], period, count)))
    end
    return body
end

baremodule SpectreEnvironment

import ..Base
import ..CedarSim
import ..CedarSim: vcvs, vccs, Switch
import ForwardDiff
import Compat
import Distributions
import StaticArrays

import Base:
    +, *, -, ==, !=, /, ^, >, <,  <=, >=,
    max, min, abs,
    log, exp, sqrt,
    sinh, cosh, tanh,
    sin, cos, tan,
    asinh, acosh, atanh,
    zero, atan,
    floor, ceil, trunc
import Base.Experimental: @overlay
import ..rem_right_semi, ..time_periodic_singularities!, ..pwl_at_time, ..wave_split

const arctan = atan
const ln = log
const pow = ^
int(x) = trunc(Int, x)
nint(x) = Base.round(Int, x)

export !, +, *, -, ==, !=, /, ^, >, <,  <=, >=,
    max, min, abs,
    ln, log, exp, sqrt,
    sinh, cosh, tanh,
    sin, cos, tan, atan, arctan,
    asinh, acosh, atanh,
    int, nint, floor, ceil, pow


const resistor = CedarSim.SimpleResistor
const capacitor = CedarSim.SimpleCapacitor
const inductor = CedarSim.SimpleInductor
const vsource = CedarSim.VoltageSource
const isource = CedarSim.CurrentSource
const diode = CedarSim.SimpleDiode
const UnimplementedDevice = CedarSim.UnimplementedDevice
const Gnd = CedarSim.Gnd

# bsource is weird. It can basically be any circuit element.
# This maps to the appropriate element, based on the keyword arguments
function bsource(;kwargs...)
    keys = Base.keys(kwargs)
    if Base.in(:v, keys)
        return vsource(tran=kwargs[:v])
    elseif Base.in(:i, keys)
        return isource(tran=kwargs[:i])
    elseif Base.in(:r, keys)
        return resistor(r=kwargs[:r])
    elseif Base.in(:c, keys)
        return capacitor(c=kwargs[:c])
    else
        cedarerror("BSOURCE with args $kwargs not supported.")
    end
end

const M_1_PI = 1/Base.pi

function pwl(wave)
    ts, ys = wave_split(wave)
    # Notify singularities at each of our timepoints
    time_periodic_singularities!(ts)

    # Actually calculate the value to return
    return pwl_at_time(ts, ys, var"$time"())
end

function pulse(v1, v2, td, tr, tf, pw=Base.Inf, period=Base.Inf, count=-1)
    ts = StaticArrays.@SVector[
        td, td+tr, td+tr+pw, td+tr+pw+tf,
    ]
    ys = StaticArrays.@SVector[
        v1, v2, v2, v1,
    ]
    # Notify singularities at each of our timepoints, repeat forever
    time_periodic_singularities!(ts, period, count)

    # Calculate value modulo our period
    t = rem_right_semi(CedarSim.spec[].time, period)
    return pwl_at_time(ts, ys, t)
end

# don't pirate Base.sin
function spsin(vo, va, freq, td=0, theta=0, phase=0, ncyles=Base.Inf)
    # see https://ltwiki.org/LTspiceHelp/LTspiceHelp/V_Voltage_Source.htm
    if td < var"$time"() < ncyles/freq
        vo+va*Base.exp(-(var"$time"()-td)*theta)*Base.sind(360*freq*(var"$time"()-td)+phase)
    else
        vo + va*Base.sind(phase)
    end
end

function agauss(nom, avar, sigma)
    rng = CedarSim.spec[].rng
    if rng === nothing
        nom
    else
        d = Distributions.Normal(0.0, avar)
        rn = Base.@noinline Base.rand(rng, d)
        nom + rn/sigma
    end
end

# Gets replaced by simulator time in the compiler override
function var"$time"()
    if CedarSim.sim_mode[] === :dcop || CedarSim.sim_mode[] === :tranop
        return 0.0
    else
        return CedarSim.spec[].time
    end
end

temper() = CedarSim.undefault(CedarSim.spec[].temp) # Celsius

const dc = :dc
const ac = :ac
const tran = :tran
export resistor, capacitor, inductor, vsource, isource, bsource, vcvs, vccs, UnimplementedDevice,
    M_1_PI, dc, ac, tran, pwl, pulse, spsin, var"$time", Gnd, agauss, temper

var"$scale"() = CedarSim.undefault(CedarSim.options[].scale)

end # baremodule SpectreEnvironment

struct BinnedModel{B<:Tuple}
    scale::Float64
    bins::B
    BinnedModel(scale, bins::B) where B = new{B}(float(scale), bins)
end

const ParsedNT = NamedTuple{names, types} where {names, types<:Tuple{Vararg{Union{DefaultOr{Int}, DefaultOr{Float64}, DefaultOr{Bool}}}}}
struct ParsedModel{T}
    model::T
end
function ParsedModel(model, kwargs)
    ParsedModel{model}(model(;kwargs...))
end

Base.show(io::IO, m::ParsedModel) = print(io, "ParsedModel($(m.model), ...)")
Base.nameof(m::ParsedModel{T}) where T = nameof(T)
Base.nameof(m::BinnedModel) = nameof(first(m.bins))

modelfields(m) = ()
modelfields(m::DataType) = fieldnames(m)
modelfields(::Type{ParsedModel{T}}) where T = modelfields(T)
modelfields(::Type{BinnedModel{T}}) where T = modelfields(eltype(T))

Base.@assume_effects :foldable function case_adjust_kwargs_fallback(model::Type{T}, kwargs::NamedTuple{Names}) where {Names, T}
    case_insensitive = Dict(Symbol(lowercase(String(kw))) => kw for kw in fieldnames(T))
    pairs = Pair[]
    for kw in (Names::Tuple{Vararg{Symbol}})
        push!(pairs, get(case_insensitive, Symbol(lowercase(String(kw))), kw)=>getfield(kwargs, kw))
    end
    (; pairs...)
end

function _case_adjust_kwargs(model::Type{T}, kwargs::NamedTuple{Names}) where {Names, T}
    if @generated
        case_insensitive = Dict(Symbol(lowercase(String(kw))) => kw for kw in fieldnames(T))
        return :((;$(map(Names) do kw
            Expr(:kw,
                get(case_insensitive, Symbol(lowercase(String(kw))), kw),
                Expr(:call, :getfield, :kwargs, quot(Symbol(kw))))
        end...)))
    else
        return case_adjust_kwargs_fallback(model, kwargs)
    end
end

"""
    case_adjust_kwargs(model, kwargs)

Adjust the case of `kwargs` (which are assumed to be all lowercase) to match the
case of the fieldnames of `model`.
"""
Base.@assume_effects :total function case_adjust_kwargs(model::Type, kwargs::ParsedNT)
    #_uppercase_kwargs(model, kwargs)::NamedTuple{<:Any, types}
    _case_adjust_kwargs(model, kwargs)::ParsedNT
end

Base.@assume_effects :total function case_adjust_kwargs(model::Type, kwargs::NamedTuple)
    #_uppercase_kwargs(model, kwargs)::NamedTuple{<:Any, types}
    _case_adjust_kwargs(model, kwargs)::NamedTuple
end

function (pm::ParsedModel)(;kwargs...)
    setproperties(pm.model, values(kwargs))
end

struct NoBinExpection <: CedarException
    bm::BinnedModel
    l::Float64
    w::Float64
end
Base.showerror(io::IO, bin::NoBinExpection) = print(io, "NoBinExpection: no bin for BinnedModel $(typeof(bin.bm)) of size (l=$(bin.l), w=$(bin.w)).")

Base.@assume_effects :consistent :effect_free :terminates_globally @noinline function find_bin(bm::BinnedModel, l, w)
    l = bm.scale*l
    w = bm.scale*w
    for bin in bm.bins
        (; LMIN, LMAX, WMIN, WMAX) = bin.model
        if undefault(LMIN::DefaultOr{Float64}) <= l < undefault(LMAX::DefaultOr{Float64}) && undefault(WMIN::DefaultOr{Float64}) <= w < undefault(WMAX::DefaultOr{Float64})
            return bin
        end
    end
    throw(NoBinExpection(bm, l, w))
end

function (bm::BinnedModel)(; l, w, kwargs...)
    find_bin(bm, l, w)(; l, w, kwargs...)
end

"Instantiate a model using SPICE case insensitive semantics"
function spicecall(model; m=1.0, kwargs...)
    ParallelInstances(model(;kwargs...), m)
end

@Base.assume_effects :foldable function mknondefault_nt(nt::NamedTuple)
    if @generated
        names = Base._nt_names(nt)
        types = Any[]
        args = Any[]
        for i = 1:length(names)
            T = fieldtype(nt, i)
            arg = :(getfield(nt, $i))
            if T <: DefaultOr
                push!(args, arg)
            else
                push!(args, Expr(:new, DefaultOr{T}, arg, false))
                T = DefaultOr{T}
            end
            push!(types, T)
        end
        nttypes = Tuple{types...}
        Expr(:new, :(NamedTuple{$names, $nttypes}), args...)
    else
        map(mknondefault, nt)
    end
end

function spicecall(pm::ParsedModel{T}; m=1, kwargs...) where T
    instkwargs = case_adjust_kwargs(T, mknondefault_nt(values(kwargs)))::ParsedNT
    inst = setproperties(pm.model, instkwargs)
    ParallelInstances(inst, m)
end

function spicecall(bm::BinnedModel; l, w, kwargs...)
    spicecall(find_bin(bm, l, w); l, w, kwargs...)
end

spicecall(::Type{ParsedModel}, model, kwargs) = ParsedModel(model, case_adjust_kwargs(model, kwargs))

export SpectreEnvironment
