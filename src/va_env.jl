# This gets imported by all generated VerilogA code. The function names
# exported here should correspond to what is made available by the VerilogA
# standard.
baremodule VerilogAEnvironment

import ..Base
import ..CedarSim
import ForwardDiff
import Compat
import NaNMath
import ChainRules
import DAECompiler
import DAECompiler: ddt

import Base:
    +, *, -, ==, !=, /, >, <,  <=, >=,
    max, min, abs,
    exp,
    sinh, cosh, tanh,
    zero, atan, floor, %, NaN

export !, +, *, -, ==, !=, /, ^, var"**", >, <,  <=, >=,
    max, min, abs,
    exp, sqrt,
    sinh, cosh, tanh,
    sin, cos, tan, atan,
    floor, %, NaN

using Base: @inbounds, @inline, @noinline
using Base.Experimental: @overlay
export @inbounds, @inline, @overlay, var"$temperature"

export pow, ln, ddt, flicker_noise, white_noise, atan2, log

@noinline Base.@assume_effects :total pow(a, b) = NaNMath.pow(a, b)
@noinline Base.@assume_effects :total pow(a::ForwardDiff.Dual, b) = NaNMath.pow(a, b)
@noinline Base.@assume_effects :total ln(x) = NaNMath.log(x)
log(x) = cedarerror("log not supported, use $log10 or $ln instead")
!(a) = Base.:!(a)
!(a::Int64) = a == zero(a)
atan2(x,y) = Base.atan(x,y)
var"**"(a, b) = pow(a, b)
^ = Base.:(⊻)
@noinline Base.@assume_effects :total sqrt(x) = NaNMath.sqrt(x)
@noinline Base.@assume_effects :total sin(x) = NaNMath.sin(x)
@noinline Base.@assume_effects :total cos(x) = NaNMath.cos(x)
@noinline Base.@assume_effects :total tan(x) = Base.tan(x)

function ChainRules.frule((_, Δx), ::typeof(sqrt), x)
    Ω = sqrt(x)
    (Ω, Δx / (2Ω))
end
function ChainRules.frule((_, Δx), ::typeof(ln), x)
    (ln(x), Δx / x)
end

function ChainRules.frule((_, Δx, Δp), ::typeof(pow), x::Number, p::Number)
    y = pow(x, p)
    _dx = ChainRules._pow_grad_x(x, p, ChainRules.float(y))
    if ChainRules.iszero(Δp)
        # Treat this as a strong zero, to avoid NaN, and save the cost of log
        return y, _dx * Δx
    else
        # This may do real(log(complex(...))) which matches ProjectTo in rrule
        _dp = ChainRules._pow_grad_p(x, p, ChainRules.float(y))
        return y, ChainRules.muladd(_dp, Δp, _dx * Δx)
    end
end

function ChainRules.frule((_, Δx), ::typeof(cos), x::Number)
    sinx, cosx = NaNMath.sincos(x)
    return (cosx, -sinx * Δx)
end

ChainRules.@scalar_rule tan(x) 1 + Ω * Ω

# Branch voltagesa
#function V(V₊, V₋)
#    return V₊.V - V₋.V
#end
#V(node) = node.V
function white_noise(dscope, pwr, name)
    DAECompiler.observed!(pwr, CedarSim.DScope(dscope, Symbol(name, :pwr)))
    DAECompiler.epsilon(CedarSim.DScope(dscope, Symbol(name)))
end
function flicker_noise(dscope, pwr, exp, name)
    DAECompiler.observed!(pwr, CedarSim.DScope(dscope, Symbol(name, :pwr)))
    DAECompiler.observed!(exp, CedarSim.DScope(dscope, Symbol(name, :exp)))
    DAECompiler.epsilon(CedarSim.DScope(dscope, Symbol(name)))
end

vaconvert(T::Type{<:Number}, x::CedarSim.Default) = CedarSim.Default(vaconvert(T, x.val))
vaconvert(T::Type{<:Number}, x::CedarSim.DefaultOr) = CedarSim.DefaultOr(vaconvert(T, x.val), x.is_default)
vaconvert(T::Type{<:Number}, x::Integer) = Base.convert(T, x)
vaconvert(::Type{<:Number}, x::Number) = x

"""
    vaconvert(::Type{Int}, x::Real)

Implements conversion of VA `real` to `integer` types.

VA-LRM 4.2.1.1 Real to integer conversion:

    If the fractional part of the real number is exactly 0.5, it shall be
    rounded away from zero.
"""
vaconvert(::Type{Int}, x::Real) = Base.round(Int, x, Base.RoundNearestTiesAway)
vaconvert(::Type{Int}, x::Integer) = x
vaconvert(T::Type{Int}, x::CedarSim.Default) = CedarSim.Default(vaconvert(T, x.val))
vaconvert(T::Type{Int}, x::CedarSim.DefaultOr) = CedarSim.DefaultOr(vaconvert(T, x.val), x.is_default)

export var"$simparam"

var"$simparam"(param) = CedarSim.undefault(Base.getproperty(CedarSim.spec[], Symbol(param)))
function var"$simparam"(param, default)
    if Base.hasproperty(CedarSim.spec[], Symbol(param))
        return CedarSim.undefault(Base.getproperty(CedarSim.spec[], Symbol(param)))
    else
        return default
    end
end

var"$temperature"() = CedarSim.undefault(CedarSim.spec[].temp)+273.15 # Kelvin

abstract type VAModel <: CedarSim.CircuitElement; end

end
