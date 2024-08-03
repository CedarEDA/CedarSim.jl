# Implementing devices

!!! warning

    This documentation describes CedarSim internals. At the moment, this is not supported as
    a stable API, but is intended to help users understand how Cedar works and to aid in
    experimentation. For end-users needing custom devices, Verilog-A is the recommended model
    representation.

!!! warning

    Device equations may be specified in a subset of valid Julia. However, this subset is currently
    not precisely specified and no tooling is currently available to verify whether or not a device
    implementation complies with the supported subset.

## Basic concepts

In the previous chapter, we saw how circuits can be represented as julia functions. However,
the circuits we defined were still composed of the basic primitive SPICE devices. In this
chapter, we will explore the implementation of these devices and how to implement new kinds
of devices or model representations. The basic idea is to implement the mathematical equations
for a particular device in terms of the primitives provided by DAECompiler. See the DAECompiler
docs for a detailed description of these intrinsic.

To begin, we will study some illustrative and simplified device implementations, gradually building
up some additional features and complexity.

# A basic resistor
To begin with, consider a resistor:

```
struct MyResistor
    resistance::Float64
end

(R::MyResistor)(A, B) = branch!((V, I)->V - I*R.resistance, A, B)
```

Here, we use the CedarSim `branch!` helper, which is used to introduce a branch between two nodes
and is very helpful for implementing two terminal devices. `branch!` provides the voltage
difference across, as well as the current through the branch to the provided callback. This callback
in turn, should implement (and return) the relation that must hold between voltage and current
for this device. In the case of a resistor, this is simply `V = I*R` or as expressed here `0 = V - I*R`.
This is a fully functional implementation of a resitor. CedarSim and DAECompiler will figure out
everything else.

# Using custom devices from a SPICE circuit

Now that we have a custom device, how do we use it? The first option is to simply use it directly in
a julia-defined circuit as we saw in the previous chapter:

```
using CedarSim.DeviceShorthands
using CedarEDA.SIFactors: k, μ

circuit() = (MyResistor(1k) ∥ C(1μ))(net(), gnd())
```

This is equivalent to the RC example we saw at the end of the previous chapter.
However, as written, the variable representing the current through the resistor is unnamed and can
thus not be referenced symbolically. To fix this, and allow our custom device to participate in
the `"R1"(MyResistor(1k))` syntax for providing device names, we need to slightly tweak our
device definition to subtype `CircuitElement` (which provides the string overload) and thread through
the debug scope (which is used to give the branch a symbolic name):

```
using CedarSim.DeviceUtils
struct MyResistor <: CircuitElement
    resistance::Float64
end

(R::MyResistor)(A, B; dscope=:R) = branch!((V, I)->V - I*R.resistance, dscope, A, B)
```

With this definition, even without providing a name, we can query the current through the resistor as
`sys.R1`. The `:R` default we provided to `dscope` means that all devices of this type will default
to names starting with `R` - the names are stable for a particular circuit definition, but can change
arbitrarily when the circuit is modified, so providing explicit names is recommended. See the circuit
definition docs for further details.

With this set, we are now also ready to use this device from SPICE. The simplest way to do so is to use
the string interpolation syntax:

```julia
using CedarSim.DeviceShorthands
function circuit()
    sp"""
    * A simple RC circuit with a custom resistor device
    R1 1 0 $(MyResistor(1k))
    C1 1 0 1u
    """e
end
```

custom devices may be used in model position for any SPICE device type. No extra parameters should be passed on
the SPICE instantiation line, as parameters are provided directly to the julia constructor.

# Using DAECompiler intrinsics

So far, we've seen how to use the `branch!` abstraction provided by CedarSim. This is sufficient for many linear
and non-linear devices whose constituent equations are simply a function of the branch voltage and current.
However, if the device has additional internal state, you may need to introduce additional variables directly
using the DAECompiler API. In this example, we shall consider a resistor with self-heating thermal effects.
In particular, we will model:

- The current through the resistor increasing the temperature
- Radiative and other thermal losses to the environment
- Thermal effects on the resistance of the resistor

Note that in general, one may want to instead build a generic thermal resistor with a thermal port that can
be connected for a full multi-physics simulation, but that is outside the scope of this tutorial.

```
struct ThermalResistor
    "Nominal resistance of the resistor at room temperature (20 °C)"
    R₀::Float64
    "Thermal capacitance of the resistor in J/K"
    Cth::Float64
    "Temperature coefficient of the resistor in Ω/K"
    TR::Float64
    "Thermal loss coefficient to the environment in 1/s"
    k::Float64
end

function (this::ThermalResistor)(A, B, dscope=defaultscope(:TR))
    branch!(dscope, A, B) do V, I
        # Introduce a new variable to hold the current temperature of the resistor
        (; T) = variables(dscope)
        # Instantaneous resistance
        R = this.R₀ + (T - 293.15) * this.TR
        # Instantaneous power dissipation
        P = R * I^2
        # Self-heating and radiative losses
        equation!(ddt(T) - this.Cth * P + this.k * (T - var"$temperature"()))
        # Ohm's law
        return V - I * R
    end
end
```

A couple of aspects here deserve explanation. First, we used `CedarSim.defaultscope` to
create a scope for our variables. The higher-level CedarSim `branch!` can take either
a scope or a raw symbol (in which case it will call `defaultscope` internally).
However, for raw DAECompiler APIs, we need a reference to the scope that `branch!`
would have otherwise created implicitly, so we explicitly call `defaultscope` here.

With this in hand, we used the `variables` helper from DAECompiler to introduce a variable
for our resistor temperature. We could have also used a lower level `variable(dscope(:T))`
call here, but `variables` conveniently allows us to avoid writing the variable name twice
(as `nets` would in CedarSim).

Next we used DAECompiler's `ddt` to write the equation for the derivative of our temperature.

Lastly, we used CedarSim's `var"$temperature"` to access the declared ambient temperature of
the simulation.

Note that we could have also used `V*I` for the power disscipation and calculated the resistance
last. Such a rearrangement is semantically equivalent and in fact DAECompiler will happily
rearrange such equations according to its heuristics.

# The implementation of `branch!`

Finally, we're ready to see how `branch!` itself is implemented. For two terminal devices,
it is recommended to just use `branch!` directly, but for devices with more terminals,
it can be useful to work directly with the underlying nets.

First, here is the implementation of `branch!`:

```
function branch!(scope::AbstractScope, net₊::AbstractNet, net₋::AbstractNet)
    # Branch current - semantically flows from net₊ to net₋
    I = variable(scope(:I))
    kcl!(net₊, -I)
    kcl!(net₋,  I)
    V = net₊.V - net₋.V
    observed!(ForwardDiff.value(SimTag, V), scope(:V))
    (V, I)
end
```

Here we used another CedarSim abstraction, `kcl!` to add the current contributions into
the KCL for each for the two nets. Using the abstraction is recommended in case of
any future changes to the implementation of `Net`. However, for completeness, here is
(a slightly simplified version of) the current implementation:

```
struct Net{T} <: AbstractNet
    V::T
    kcl!::equation
    function Net(name::AbstractScope)
        V = variable(name)
        kcl! = equation(name)
        return new(V, kcl!)
    end
end
kcl!(net::AbstractNet, current) = net.kcl!(current)
```

Here we are simply making use of the capability of DAECompiler to split the contributions
to an equation across multiple invocations to that equation (in particular, this use case
was motivating for that feature).

# Using ModelingToolkit
You can implement also devices using [ModelingToolkit.jl](https://github.com/SciML/ModelingToolkit.jl) and the [ModelingToolkitStandardLibary.jl](https://docs.sciml.ai/ModelingToolkitStandardLibrary/stable/), and then connect them to CedarSim circuits.
For many users this will be the best way to implement custom devices, as you can fully use the MTK ecosystem to develop and debug them in isolation then hook them up to a greater circuit in Cedar.

```@docs; canonical=false
@declare_MSLConnector
```

