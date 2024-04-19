# This file contains utility functions when writing netlists using the operadic
# julia embedding.

export ∥, ⋯, parallel, series, nets

## Diagrammatic Composition
"""
    ∥(...)
    parallel(args...)

The parallel diagrammatic composition operator `∥` (\\parallel or using
the ASCII name `parallel`) may be used for the parallel composition of
subcircuits or devices.

# Examples

```julia
# Two resistors in parallel.
const R500 = R(1k) ∥ R(1k) 

# Array of LEDs for output signals
∥(Fix1.(LED, outputs)...)(G)
```
"""
function ∥(devices...)
    (args...)->foreach(devices) do dev
        dev(args...)
    end
end
const parallel = ∥

"""
    ⋯(...)
    series(args...)

The sequential diagrammatic composition operator `⋯` (\\cdots or using
the ASCII name `series`) may be used to sequentially compose circuit elements.

# Examples

```julia
# Two resistors in series.
const R2k = R(1k) ⋯ R(1k) 

# Inverter chain
inverter(vdd, vss) = (in, out)->(pmos(vdd, in, out, vdd); nmos(out, in, vss, vss))
inverter_chain(n) = (vdd, vss, in, out)->mapreduce(_->inverter(vdd, vss), ⋯, 1:n)(in, out)
```
"""
function series(dev1, devs...)
    (l, r)->dev1(l, foldr(devices; init=r) do (dev, nr)
        nl = Net()
        dev(nl, nr)
        nl
    end)
end
const ⋯ = series

## Named devices shorthand
function (name::String)(dev::CircuitElement)
    Named(dev, name)
end

## Net spider
"""
    nets()

The `nets` helper provides shorthand syntax for introducing new nets using
destructuring syntax. It is the net analogue to `DAECompiler.variables`.

# Example

Instead of writing:
```
function my_rc_circuit()
    A = net(:A)
    B = net(:B)
    (R(1k) ∥ C(1μ))(A, B)
    Gnd(B)
end
```

You can use this to write:
```
function my_rc_circuit()
    (; A, B) = nets()
    (R(1k) ∥ C(1μ))(A, B)
    Gnd(B)
end
```

or

```
function my_rc_circuit()
    n = nets()
    (R(1k) ∥ C(1μ))(n.A, n.B)
    Gnd(n.B)
end
```
"""
mutable struct nets
    ns::NamedTuple{names, T} where {N, names, T<:NTuple{N, Net}}
    nets() = new((;))
end

Base.setproperty!(mv::nets, s::Symbol, @nospecialize(v)) = Base.setfield!(mv, s, v)
@eval @inline function Base.getproperty(mv::nets, s::Symbol)
    nt = getfield(mv, :ns)
    if hasfield(typeof(nt), s)
        return getfield(nt, s)
    else
        v = net(s)
        NT = DAECompiler.Intrinsics._compute_new_nt_type(nt, s)
        tup = tuple(nt..., v)
        NT = NT{typeof(tup)}
        mv.ns = $(Expr(:splatnew, :NT, :tup))
        return v
    end
end

## Device shorthands
module DeviceShorthands
    import ..CedarSim
    export R, L, C, t, V, I

    const R = CedarSim.SimpleResistor
    const C = CedarSim.SimpleCapacitor
    const L = CedarSim.SimpleInductor
    const t = CedarSim.SpectreEnvironment.var"$time"
    const V(v) = CedarSim.VoltageSource(dc=v)
    const I(i) = CedarSim.CurrentSource(dc=i)
end

## DeviceUtils
module DeviceUtils
    import ..CedarSim: CircuitElement, branch!
    export CircuitElement, branch!
end