var documenterSearchIndex = {"docs":
[{"location":"circuits/#Circuit-representation","page":"Circuit representation","title":"Circuit representation","text":"","category":"section"},{"location":"circuits/#CedarSim-as-a-compiler","page":"Circuit representation","title":"CedarSim as a compiler","text":"","category":"section"},{"location":"circuits/","page":"Circuit representation","title":"Circuit representation","text":"The core functionality provided by CedarSim is to compile various circuit-specfific input formats into an executable representation that is then passed on to the backend for further processing. For simulation, this backend is generally DAECompiler, but other backends are possible for other analysis tasks. CedarSim is designed to, as much as is feasible, re-use and integrate with the capabilities of the Julia compiler. As such, CedarSim uses Julia code as its executable representation. As a result, you may, and it is occasionally helpful, to think about CedarSim as a custom frontend for the Julia language that parses Julia code that happens to be written in languages that are not Julia.","category":"page"},{"location":"circuits/#Basic-circuits","page":"Circuit representation","title":"Basic circuits","text":"","category":"section"},{"location":"circuits/","page":"Circuit representation","title":"Circuit representation","text":"CedarSim is ordinarily driven by Spectre or SPICE netlists. However, it is possible to construct the executable representation manually. It is perhaps easiest to see this with an example:","category":"page"},{"location":"circuits/","page":"Circuit representation","title":"Circuit representation","text":"using CedarSim\n\n# Create a 1kΩ resistor device template\nconst R1k = CedarSim.SimpleResistor(1000.)\n\n# Cretae a 1μF capacitor device template\nconst C1μ = CedarSim.SimpleCapacitor(1e-6)\n\n# Create a Gnd device template\nconst G = CedarSim.Gnd()\n\nfunction my_rc_circuit()\n    # Create two nets\n    A = net()\n    B = net()\n\n    # Wire up the circuit\n    R1k(A, B)\n    C1μ(A, B)\n    G(B)\nend","category":"page"},{"location":"circuits/","page":"Circuit representation","title":"Circuit representation","text":"Running this \"circuit\" as a julia function returns nothing, but doing so can be useful as a sanity check that there are no typos in the definition:","category":"page"},{"location":"circuits/","page":"Circuit representation","title":"Circuit representation","text":"my_rc_circuit()","category":"page"},{"location":"circuits/","page":"Circuit representation","title":"Circuit representation","text":"This function fully represents a circuit and may be used in all APIs that expect circuits:","category":"page"},{"location":"circuits/","page":"Circuit representation","title":"Circuit representation","text":"using OrdinaryDiffEq, UnicodePlots\n\nsys = CircuitIRODESystem(my_rc_circuit)\n\nsol = solve(ODEProblem(sys, [1.0], (0., 1e-2)), Rosenbrock23(; autodiff=false))\n\nlineplot(sol.t, sol[sys.C1.V])","category":"page"},{"location":"circuits/","page":"Circuit representation","title":"Circuit representation","text":"!!! warning This method of plotting is useful for quick validation, but not recommended. In particular, we are ignoring the solver's internal interpolation, instead substituting the plotting library's linear interpolation. For proper plots, please use the higher-level plotting APIs in CedarEDA.","category":"page"},{"location":"circuits/#Names","page":"Circuit representation","title":"Names","text":"","category":"section"},{"location":"circuits/","page":"Circuit representation","title":"Circuit representation","text":"In the above example, we were able to access the capacitor using the name C1 even though no name was specified. When no name is specified, the backend will assign unique names after all other names have been resolved. This process is deterministic for a particular function/environment (library versions, etc.), but of course small changes to the circuit can completely change the auto-generated names.","category":"page"},{"location":"circuits/","page":"Circuit representation","title":"Circuit representation","text":"To ensure stable names, names may be assigned explicitly using the Named wrapper:","category":"page"},{"location":"circuits/","page":"Circuit representation","title":"Circuit representation","text":"function my_named_rc_circuit()\n    # Create two nets\n    A = net()\n    B = net()\n\n    # Wire up the circuit\n    Named(R1k, \"R\")(A, B)\n    Named(C1μ, \"MyCapacitor\")(A, B)\n    G(B)\nend","category":"page"},{"location":"circuits/","page":"Circuit representation","title":"Circuit representation","text":"With these names set, we can now access the capacitor states sys.MyCapacitor. For basic devices and subcircuits (everything that is ::CircuitElement), there is also a shorthand overload that omits Named (but is otherwise exactly equivalent):","category":"page"},{"location":"circuits/","page":"Circuit representation","title":"Circuit representation","text":"function my_named_rc_circuit2()\n    # Create two nets\n    A = net()\n    B = net()\n\n    # Wire up the circuit\n    \"R\"(R1k)(A, B)\n    \"MyCapacitor\"(C1μ)(A, B)\n    G(B)\nend","category":"page"},{"location":"circuits/#Some-additional-conveniences","page":"Circuit representation","title":"Some additional conveniences","text":"","category":"section"},{"location":"circuits/","page":"Circuit representation","title":"Circuit representation","text":"In the above, we have named our devices, but the nets remain yet unnamed. There are several options for naming nets:","category":"page"},{"location":"circuits/","page":"Circuit representation","title":"Circuit representation","text":"The name of a net may be passed as the first argument to net\nThey may be named using device syntax\nThe nets helper can be used to automatically name a net and assign it to the corresponding variable.","category":"page"},{"location":"circuits/","page":"Circuit representation","title":"Circuit representation","text":"In other words, these three are all equivalent:","category":"page"},{"location":"circuits/","page":"Circuit representation","title":"Circuit representation","text":"A = net(:A)\n# A = \"A\"(net)() # XXX\n(;A) = nets()","category":"page"},{"location":"circuits/","page":"Circuit representation","title":"Circuit representation","text":"There are some addditional convenience available.","category":"page"},{"location":"circuits/#Gnd-special-case","page":"Circuit representation","title":"Gnd special case","text":"","category":"section"},{"location":"circuits/","page":"Circuit representation","title":"Circuit representation","text":"CedarSim exports the function:","category":"page"},{"location":"circuits/","page":"Circuit representation","title":"Circuit representation","text":"gnd() = (g = net(); Gnd()(g); g)","category":"page"},{"location":"circuits/","page":"Circuit representation","title":"Circuit representation","text":"which can be used as a covenient shorthand to obtain ground for situations where it's more convenient to think of ground as a net rather than a device.","category":"page"},{"location":"circuits/#CedarSim.DeviceShorthands","page":"Circuit representation","title":"CedarSim.DeviceShorthands","text":"","category":"section"},{"location":"circuits/","page":"Circuit representation","title":"Circuit representation","text":"The DeviceShorthands submodule exports short aliases for the basic SPICE devices using function names that (in general) match their SPICE prefix. These can be convenient for quickly writing netlists. For example, resistors and capacitors are available using the DeviceShorthands.R and DeviceShorthands.C functions respectively.","category":"page"},{"location":"circuits/#Parallel-and-series-composition-operators","page":"Circuit representation","title":"Parallel and series composition operators","text":"","category":"section"},{"location":"circuits/","page":"Circuit representation","title":"Circuit representation","text":"The operators ∥ and ⋯ (typed using \\parallel and \\cdots; also availbel using the ASCII names parallel and series) may be used for parallel and sequential composition respectively.","category":"page"},{"location":"circuits/","page":"Circuit representation","title":"Circuit representation","text":"For example, our running RC circuit example, could have been simply written as:","category":"page"},{"location":"circuits/","page":"Circuit representation","title":"Circuit representation","text":"using CedarSim.DeviceShorthands\n\nusing CedarEDA.SIFactors: k, μ\n\nRC() = (R(1k) ∥ C(1μ))(net(), gnd())","category":"page"},{"location":"circuits/#Subcircuits","page":"Circuit representation","title":"Subcircuits","text":"","category":"section"},{"location":"circuits/","page":"Circuit representation","title":"Circuit representation","text":"Regular function composition does not by default introduce subcircuits. For example, the function:","category":"page"},{"location":"circuits/","page":"Circuit representation","title":"Circuit representation","text":"RC2() = (RC(); RC())","category":"page"},{"location":"circuits/","page":"Circuit representation","title":"Circuit representation","text":"described a circuit with four top level devices (sys.R1, sys.R2, sys.C1, sys.C2). To instead create a hierachichal circuit, CedarSim provides the SubCircuit device.","category":"page"},{"location":"circuits/","page":"Circuit representation","title":"Circuit representation","text":"RC2_sub() = (SubCircuit(RC)(); SubCircuit(RC)();)","category":"page"},{"location":"circuits/","page":"Circuit representation","title":"Circuit representation","text":"The names of the four devices are now sys.X1.R1, sys.X1.C1, sys.X2.R1, sys.X2.C1.","category":"page"},{"location":"circuits/","page":"Circuit representation","title":"Circuit representation","text":"note: Note\nThe use of subcircuits is a convenience for the user to allow hierarchical specification of device names. The compiler takes advantage of hierachy whether or not the SubCircuit device is used.","category":"page"},{"location":"devices/#Implementing-devices","page":"Implementing devices","title":"Implementing devices","text":"","category":"section"},{"location":"devices/","page":"Implementing devices","title":"Implementing devices","text":"warning: Warning\nThis documentation describes CedarSim internals. At the moment, this is not supported as a stable API, but is intended to help users understand how Cedar works and to aid in experimentation. For end-users needing custom devices, Verilog-A is the recommended model representation.","category":"page"},{"location":"devices/","page":"Implementing devices","title":"Implementing devices","text":"warning: Warning\nDevice equations may be specified in a subset of valid Julia. However, this subset is currently not precisely specified and no tooling is currently available to verify whether or not a device implementation complies with the supported subset.","category":"page"},{"location":"devices/#Basic-concepts","page":"Implementing devices","title":"Basic concepts","text":"","category":"section"},{"location":"devices/","page":"Implementing devices","title":"Implementing devices","text":"In the previous chapter, we saw how circuits can be represented as julia functions. However, the circuits we defined were still composed of the basic primitive SPICE devices. In this chapter, we will explore the implementation of these devices and how to implement new kinds of devices or model representations. The basic idea is to implement the mathematical equations for a particular device in terms of the primitives provided by DAECompiler. See the DAECompiler docs for a detailed description of these intrinsic.","category":"page"},{"location":"devices/","page":"Implementing devices","title":"Implementing devices","text":"To begin, we will study some illustrative and simplified device implementations, gradually building up some additional features and complexity.","category":"page"},{"location":"devices/#A-basic-resistor","page":"Implementing devices","title":"A basic resistor","text":"","category":"section"},{"location":"devices/","page":"Implementing devices","title":"Implementing devices","text":"To begin with, consider a resistor:","category":"page"},{"location":"devices/","page":"Implementing devices","title":"Implementing devices","text":"struct MyResistor\n    resistance::Float64\nend\n\n(R::MyResistor)(A, B) = branch!((V, I)->V - I*R.resistance, A, B)","category":"page"},{"location":"devices/","page":"Implementing devices","title":"Implementing devices","text":"Here, we use the CedarSim branch! helper, which is used to introduce a branch between two nodes and is very helpful for implementing two terminal devices. branch! provides the voltage difference across, as well as the current through the branch to the provided callback. This callback in turn, should implement (and return) the relation that must hold between voltage and current for this device. In the case of a resistor, this is simply V = I*R or as expressed here 0 = V - I*R. This is a fully functional implementation of a resitor. CedarSim and DAECompiler will figure out everything else.","category":"page"},{"location":"devices/#Using-custom-devices-from-a-SPICE-circuit","page":"Implementing devices","title":"Using custom devices from a SPICE circuit","text":"","category":"section"},{"location":"devices/","page":"Implementing devices","title":"Implementing devices","text":"Now that we have a custom device, how do we use it? The first option is to simply use it directly in a julia-defined circuit as we saw in the previous chapter:","category":"page"},{"location":"devices/","page":"Implementing devices","title":"Implementing devices","text":"using CedarSim.DeviceShorthands\nusing CedarEDA.SIFactors: k, μ\n\ncircuit() = (MyResistor(1k) ∥ C(1μ))(net(), gnd())","category":"page"},{"location":"devices/","page":"Implementing devices","title":"Implementing devices","text":"This is equivalent to the RC example we saw at the end of the previous chapter. However, as written, the variable representing the current through the resistor is unnamed and can thus not be referenced symbolically. To fix this, and allow our custom device to participate in the \"R1\"(MyResistor(1k)) syntax for providing device names, we need to slightly tweak our device definition to subtype CircuitElement (which provides the string overload) and thread through the debug scope (which is used to give the branch a symbolic name):","category":"page"},{"location":"devices/","page":"Implementing devices","title":"Implementing devices","text":"using CedarSim.DeviceUtils\nstruct MyResistor <: CircuitElement\n    resistance::Float64\nend\n\n(R::MyResistor)(A, B; dscope=:R) = branch!((V, I)->V - I*R.resistance, dscope, A, B)","category":"page"},{"location":"devices/","page":"Implementing devices","title":"Implementing devices","text":"With this definition, even without providing a name, we can query the current through the resistor as sys.R1. The :R default we provided to dscope means that all devices of this type will default to names starting with R - the names are stable for a particular circuit definition, but can change arbitrarily when the circuit is modified, so providing explicit names is recommended. See the circuit definition docs for further details.","category":"page"},{"location":"devices/","page":"Implementing devices","title":"Implementing devices","text":"With this set, we are now also ready to use this device from SPICE. The simplest way to do so is to use the string interpolation syntax:","category":"page"},{"location":"devices/","page":"Implementing devices","title":"Implementing devices","text":"using CedarSim.DeviceShorthands\nfunction circuit()\n    sp\"\"\"\n    * A simple RC circuit with a custom resistor device\n    R1 1 0 $(MyResistor(1k))\n    C1 1 0 1u\n    \"\"\"e\nend","category":"page"},{"location":"devices/","page":"Implementing devices","title":"Implementing devices","text":"custom devices may be used in model position for any SPICE device type. No extra parameters should be passed on the SPICE instantiation line, as parameters are provided directly to the julia constructor.","category":"page"},{"location":"devices/#Using-DAECompiler-intrinsics","page":"Implementing devices","title":"Using DAECompiler intrinsics","text":"","category":"section"},{"location":"devices/","page":"Implementing devices","title":"Implementing devices","text":"So far, we've seen how to use the branch! abstraction provided by CedarSim. This is sufficient for many linear and non-linear devices whose constituent equations are simply a function of the branch voltage and current. However, if the device has additional internal state, you may need to introduce additional variables directly using the DAECompiler API. In this example, we shall consider a resistor with self-heating thermal effects. In particular, we will model:","category":"page"},{"location":"devices/","page":"Implementing devices","title":"Implementing devices","text":"The current through the resistor increasing the temperature\nRadiative and other thermal losses to the environment\nThermal effects on the resistance of the resistor","category":"page"},{"location":"devices/","page":"Implementing devices","title":"Implementing devices","text":"Note that in general, one may want to instead build a generic thermal resistor with a thermal port that can be connected for a full multi-physics simulation, but that is outside the scope of this tutorial.","category":"page"},{"location":"devices/","page":"Implementing devices","title":"Implementing devices","text":"struct ThermalResistor\n    \"Nominal resistance of the resistor at room temperature (20 °C)\"\n    R₀::Float64\n    \"Thermal capacitance of the resistor in J/K\"\n    Cth::Float64\n    \"Temperature coefficient of the resistor in Ω/K\"\n    TR::Float64\n    \"Thermal loss coefficient to the environment in 1/s\"\n    k::Float64\nend\n\nfunction (this::ThermalResistor)(A, B, dscope=defaultscope(:TR))\n    branch!(dscope, A, B) do V, I\n        # Introduce a new variable to hold the current temperature of the resistor\n        (; T) = variables(dscope)\n        # Instantaneous resistance\n        R = this.R₀ + (T - 293.15) * this.TR\n        # Instantaneous power dissipation\n        P = R * I^2\n        # Self-heating and radiative losses\n        equation!(ddt(T) - this.Cth * P + this.k * (T - var\"$temperature\"()))\n        # Ohm's law\n        return V - I * R\n    end\nend","category":"page"},{"location":"devices/","page":"Implementing devices","title":"Implementing devices","text":"A couple of aspects here deserve explanation. First, we used CedarSim.defaultscope to create a scope for our variables. The higher-level CedarSim branch! can take either a scope or a raw symbol (in which case it will call defaultscope internally). However, for raw DAECompiler APIs, we need a reference to the scope that branch! would have otherwise created implicitly, so we explicitly call defaultscope here.","category":"page"},{"location":"devices/","page":"Implementing devices","title":"Implementing devices","text":"With this in hand, we used the variables helper from DAECompiler to introduce a variable for our resistor temperature. We could have also used a lower level variable(dscope(:T)) call here, but variables conveniently allows us to avoid writing the variable name twice (as nets would in CedarSim).","category":"page"},{"location":"devices/","page":"Implementing devices","title":"Implementing devices","text":"Next we used DAECompiler's ddt to write the equation for the derivative of our temperature.","category":"page"},{"location":"devices/","page":"Implementing devices","title":"Implementing devices","text":"Lastly, we used CedarSim's var\"$temperature\" to access the declared ambient temperature of the simulation.","category":"page"},{"location":"devices/","page":"Implementing devices","title":"Implementing devices","text":"Note that we could have also used V*I for the power disscipation and calculated the resistance last. Such a rearrangement is semantically equivalent and in fact DAECompiler will happily rearrange such equations according to its heuristics.","category":"page"},{"location":"devices/#The-implementation-of-branch!","page":"Implementing devices","title":"The implementation of branch!","text":"","category":"section"},{"location":"devices/","page":"Implementing devices","title":"Implementing devices","text":"Finally, we're ready to see how branch! itself is implemented. For two terminal devices, it is recommended to just use branch! directly, but for devices with more terminals, it can be useful to work directly with the underlying nets.","category":"page"},{"location":"devices/","page":"Implementing devices","title":"Implementing devices","text":"First, here is the implementation of branch!:","category":"page"},{"location":"devices/","page":"Implementing devices","title":"Implementing devices","text":"function branch!(scope::AbstractScope, net₊::AbstractNet, net₋::AbstractNet)\n    # Branch current - semantically flows from net₊ to net₋\n    I = variable(scope(:I))\n    kcl!(net₊, -I)\n    kcl!(net₋,  I)\n    V = net₊.V - net₋.V\n    observed!(ForwardDiff.value(SimTag, V), scope(:V))\n    (V, I)\nend","category":"page"},{"location":"devices/","page":"Implementing devices","title":"Implementing devices","text":"Here we used another CedarSim abstraction, kcl! to add the current contributions into the KCL for each for the two nets. Using the abstraction is recommended in case of any future changes to the implementation of Net. However, for completeness, here is (a slightly simplified version of) the current implementation:","category":"page"},{"location":"devices/","page":"Implementing devices","title":"Implementing devices","text":"struct Net{T} <: AbstractNet\n    V::T\n    kcl!::equation\n    function Net(name::AbstractScope)\n        V = variable(name)\n        kcl! = equation(name)\n        return new(V, kcl!)\n    end\nend\nkcl!(net::AbstractNet, current) = net.kcl!(current)","category":"page"},{"location":"devices/","page":"Implementing devices","title":"Implementing devices","text":"Here we are simply making use of the capability of DAECompiler to split the contributions to an equation across multiple invocations to that equation (in particular, this use case was motivating for that feature).","category":"page"},{"location":"#CedarSim.jl","page":"Home","title":"CedarSim.jl","text":"","category":"section"},{"location":"","page":"Home","title":"Home","text":"Documentation for CedarSim.jl.","category":"page"}]
}
