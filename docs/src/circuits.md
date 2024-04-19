# Circuit representation

## CedarSim as a compiler

The core functionality provided by CedarSim is to compile various circuit-specfific
input formats into an executable representation that is then passed on to
the backend for further processing. For simulation, this backend is generally
[DAECompiler](https://help.juliahub.com/daecompiler/stable/), but other backends
are possible for other analysis tasks. CedarSim is designed to,
as much as is feasible, re-use and integrate with the capabilities of the Julia
compiler. As such, CedarSim uses Julia code as its executable representation.
As a result, you may, and it is occasionally helpful, to think about CedarSim
as a custom frontend for the Julia language that parses Julia code that happens
to be written in languages that are not Julia.

# Basic circuits

CedarSim is ordinarily driven by Spectre or SPICE netlists. However, it is
possible to construct the executable representation manually. It is perhaps
easiest to see this with an example:

```@example basic-circuit
using CedarSim

# Create a 1kΩ resistor device template
const R1k = CedarSim.SimpleResistor(1000.)

# Cretae a 1μF capacitor device template
const C1μ = CedarSim.SimpleCapacitor(1e-6)

# Create a Gnd device template
const G = CedarSim.Gnd()

function my_rc_circuit()
    # Create two nets
    A = net()
    B = net()

    # Wire up the circuit
    R1k(A, B)
    C1μ(A, B)
    G(B)
end
```

Running this "circuit" as a julia function returns `nothing`, but doing so can
be useful as a sanity check that there are no typos in the definition:

```@example basic-circuit
my_rc_circuit()
```

This function fully represents a circuit and may be used in all APIs that expect
circuits:

```@example basic-circuit
using OrdinaryDiffEq, UnicodePlots

sys = CircuitIRODESystem(my_rc_circuit)

sol = solve(ODEProblem(sys, [1.0], (0., 1e-2)), Rosenbrock23(; autodiff=false))

lineplot(sol.t, sol[sys.C1.V])
```

!!! warning This method of plotting is useful for quick validation, but not recommended. In particular, we are ignoring the solver's internal interpolation, instead substituting the plotting library's linear interpolation. For proper plots, please use the higher-level plotting APIs in CedarEDA.

## Names

In the above example, we were able to access the capacitor using the name `C1`
even though no name was specified. When no name is specified, the backend will
assign unique names after all other names have been resolved. This process is
deterministic for a particular function/environment (library versions, etc.), but
of course small changes to the circuit can completely change the auto-generated names.

To ensure stable names, names may be assigned explicitly using the `Named` wrapper:

```@example basic-circuit
function my_named_rc_circuit()
    # Create two nets
    A = net()
    B = net()

    # Wire up the circuit
    Named(R1k, "R")(A, B)
    Named(C1μ, "MyCapacitor")(A, B)
    G(B)
end
```

With these names set, we can now access the capacitor states `sys.MyCapacitor`.
For basic devices and subcircuits (everything that is `::CircuitElement`), there
is also a shorthand overload that omits `Named` (but is otherwise exactly equivalent):

```@example basic-circuit
function my_named_rc_circuit2()
    # Create two nets
    A = net()
    B = net()

    # Wire up the circuit
    "R"(R1k)(A, B)
    "MyCapacitor"(C1μ)(A, B)
    G(B)
end
```

## Some additional conveniences

In the above, we have named our devices, but the nets remain yet unnamed. There
are several options for naming nets:

1. The name of a net may be passed as the first argument to `net`
2. They may be named using device syntax
3. The `nets` helper can be used to automatically name a net and assign it to
   the corresponding variable.

In other words, these three are all equivalent:

```@example basic-circuit
A = net(:A)
# A = "A"(net)() # XXX
(;A) = nets()
```

There are some addditional convenience available.

### Gnd special case

CedarSim exports the function:

```@example basic-circuit
gnd() = (g = net(); Gnd()(g); g)
```

which can be used as a covenient shorthand to obtain ground for situations where
it's more convenient to think of ground as a net rather than a device.

### `CedarSim.DeviceShorthands`

The `DeviceShorthands` submodule exports short aliases for the basic SPICE devices
using function names that (in general) match their SPICE prefix. These can
be convenient for quickly writing netlists. For example, resistors and capacitors
are available using the `DeviceShorthands.R` and `DeviceShorthands.C` functions
respectively.

### Parallel and series composition operators

The operators `∥` and `⋯` (typed using `\parallel` and `\cdots`; also availbel using
the ASCII names `parallel` and `series`) may be used for parallel and sequential composition
respectively.

For example, our running RC circuit example, could have been simply written as:

```@broken-example basic-circuit
using CedarSim.DeviceShorthands

using CedarEDA.SIFactors: k, μ

RC() = (R(1k) ∥ C(1μ))(net(), gnd())
```

## Subcircuits

Regular function composition does not by default introduce subcircuits. For
example, the function:

```@broken-example basic-circuit
RC2() = (RC(); RC())
```

described a circuit with four top level devices (`sys.R1`, `sys.R2`, `sys.C1`, `sys.C2`). To instead create a hierachichal
circuit, `CedarSim` provides the `SubCircuit` device.

``` @broken-example basic-circuit
RC2_sub() = (SubCircuit(RC)(); SubCircuit(RC)();)
```

The names of the four devices are now `sys.X1.R1`, `sys.X1.C1`, `sys.X2.R1`, `sys.X2.C1`.

!!! note
    The use of subcircuits is a convenience for the user to allow hierarchical
    specification of device names. The compiler takes advantage of hierachy whether
    or not the `SubCircuit` device is used.
