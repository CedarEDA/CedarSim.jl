module test_sweep

# Include all our testing packages, helper routines, etc...
using CedarSim
include(joinpath(Base.pkgdir(CedarSim), "test", "common.jl"))

# Simple two-resistor circuit:
#
#  ┌─R1─┬── +
#  V    R2
#  └────┴── -
#
# We'll vary `R1` and `R2` with our CircuitSweep API,
# then verify that the current out of `V` is correct.
@kwdef struct TwoResistorCircuit
    R1::Float64 = 1000.0
    R2::Float64 = 1000.0
end

function (self::TwoResistorCircuit)()
    vcc = Named(net, "vcc")()
    gnd = Named(net, "gnd")()
    out = Named(net, "out")()
    Named(V(1.), "V")(vcc, gnd)
    Named(R(self.R1), "R1")(vcc, out)
    Named(R(self.R2), "R2")(out, gnd)
    Named(Gnd(), "G")(gnd)
end

using CedarSim: nest_param_list, flatten_param_list
@testset "nest and flatten param lists" begin
    # Test that it works with a Dict
    param_list_dict = Dict(
        :R1 => 1,
        Symbol("x1.R3") => 2,
        Symbol("x1.x2.R1") => 3,
        Symbol("x1.x2.R2") => 4,
    )
    param_list_tuple = (
        (:R1, 1.0),
        (Symbol("x1.R3"), 2),
        (Symbol("x1.x2.R1"), 3),
        (Symbol("x1.x2.R2"), 4),
    )
    # Named tuples seem to have their own sorting method
    param_nested_namedtuple = (
        R1 = 1,
        x1 = (
            x2 = (R2 = 4, R1 = 3),
            R3 = 2,
        ),
    )
    @test nest_param_list(param_list_dict) == param_nested_namedtuple
    @test nest_param_list(param_list_tuple) == param_nested_namedtuple
    @test flatten_param_list(param_nested_namedtuple) == param_list_tuple
    @test flatten_param_list(nest_param_list(param_list_tuple)) == param_list_tuple

    # Trying to assign a prefix of another value is an error
    @test_throws CedarSim.WrappedCedarException{<:ArgumentError} CedarSim.nest_param_list((
        (Symbol("x1"), 1),
        (Symbol("x1.R1"), 2),
    ))

    # Double-assigning a value is an error:
    @test_throws CedarSim.WrappedCedarException{<:ArgumentError} CedarSim.nest_param_list((
        (Symbol("x1"), 1),
        (Symbol("x1"), 2),
    ))
end

@testset "Sweep" begin
    s = Sweep(:R1, 0.1:0.1:1.0)
    @test length(s) == 10
    @test size(s) == (10,)
    @test size(s, 1) == 10
    @test size(s, 2) == 1
    @test first(s) == ((:R1, 0.1),)

    @test Sweep(R1 = 0.1:0.1:1.0) == s
    @test first(Sweep(var"a.b" = 1.0:2.0)) == ((Symbol("a.b"), 1),)

    # Make sure that `Sweep()` with a scalar works:
    s = Sweep(a = 10.0)
    @test length(s) == 1
    @test size(s) == (1,)
    @test size(s, 1) == 1
    @test size(s, 2) == 1
    @test first(s) == ((:a, 10),)

    # Test ProductSweep
    s = ProductSweep(R1 = 1.0:2.0, R2 = 1.0:2.0, R3 = 1.0:2.0)
    @test size(s) == (2, 2, 2)
    @test size(s, 1) == 2
    @test size(s, 2) == 2
    @test size(s, 3) == 2
    @test size(s, 4) == 1
    @test first(s) == (
        (:R1, 1.0),
        (:R2, 1.0),
        (:R3, 1.0),
    )
    @test length(sweepvars(s)) == 3
    @test :R1 ∈ sweepvars(s)
    @test :R2 ∈ sweepvars(s)
    @test :R3 ∈ sweepvars(s)

    # Test empty ProductSweep
    s = ProductSweep()
    @test size(s) == ()
    @test isempty(sweepvars(s))
    @test first(s) == ()

    # Test TandemSweep
    s = TandemSweep(R1 = 1.0:2.0, R2 = 1.0:2.0, R3 = 1.0:2.0)
    @test size(s) == (2,)
    @test size(s, 1) == 2
    @test size(s, 2) == 1
    @test collect(s) == [
        ((:R1, 1.0), (:R2, 1.0), (:R3, 1.0)),
        ((:R1, 2.0), (:R2, 2.0), (:R3, 2.0)),
    ]
    @test length(sweepvars(s)) == 3
    @test :R1 ∈ sweepvars(s)
    @test :R2 ∈ sweepvars(s)
    @test :R3 ∈ sweepvars(s)

    # Test SerialSweep
    s = SerialSweep(R1 = 1.0:2.0, R2 = 1.0:2.0)
    @test size(s) == (4,)
    @test size(s, 1) == 4
    @test collect(s) == [
        ((:R1, 1.0), (:R2, nothing)),
        ((:R1, 2.0), (:R2, nothing)),
        ((:R1, nothing), (:R2, 1.0)),
        ((:R1, nothing), (:R2, 2.0)),
    ]
    @test length(sweepvars(s)) == 2
    @test :R1 ∈ sweepvars(s)
    @test :R2 ∈ sweepvars(s)

    # Test empty SerialSweep
    s = SerialSweep()
    @test size(s) == (0,)
    @test length(s) == 0
    @test isempty(sweepvars(s))

    # Test compositions of these!
    s = ProductSweep(ProductSweep(R1 = 1.0:2.0, R2 = 1.0:2.0), R3 = 1.0:2.0)
    @test all(collect(s) .==
              collect(ProductSweep(R1 = 1.0:2.0, R2 = 1.0:2.0, R3 = 1.0:2.0)))
    @test length(sweepvars(s)) == 3
    @test :R1 ∈ sweepvars(s)
    @test :R2 ∈ sweepvars(s)
    @test :R3 ∈ sweepvars(s)

    s = ProductSweep(TandemSweep(R1 = 1.0:2.0, R2 = 1.0:2.0), R3 = 1.0:2.0)
    @test collect(s) == [
        ((:R1, 1.0), (:R2, 1.0), (:R3, 1.0)) ((:R1, 1.0), (:R2, 1.0), (:R3, 2.0));
        ((:R1, 2.0), (:R2, 2.0), (:R3, 1.0)) ((:R1, 2.0), (:R2, 2.0), (:R3, 2.0));
    ]
    @test length(sweepvars(s)) == 3
    @test :R1 ∈ sweepvars(s)
    @test :R2 ∈ sweepvars(s)
    @test :R3 ∈ sweepvars(s)

    s = ProductSweep(SerialSweep(R3 = 1.0:2.0, R4 = 1.0:2.0), R1 = 1.0:2.0, R2 = 1.0:2.0)
    @test size(s) == (4, 2, 2)
    @test size(s, 1) == 4
    @test size(s, 2) == 2
    @test size(s, 3) == 2
    @test collect(s)[:] == [
        ((:R1, 1.0), (:R2, 1.0), (:R3, 1.0), (:R4, nothing))
        ((:R1, 1.0), (:R2, 1.0), (:R3, 2.0), (:R4, nothing))
        ((:R1, 1.0), (:R2, 1.0), (:R3, nothing), (:R4, 1.0))
        ((:R1, 1.0), (:R2, 1.0), (:R3, nothing), (:R4, 2.0))
        ((:R1, 2.0), (:R2, 1.0), (:R3, 1.0), (:R4, nothing))
        ((:R1, 2.0), (:R2, 1.0), (:R3, 2.0), (:R4, nothing))
        ((:R1, 2.0), (:R2, 1.0), (:R3, nothing), (:R4, 1.0))
        ((:R1, 2.0), (:R2, 1.0), (:R3, nothing), (:R4, 2.0))
        ((:R1, 1.0), (:R2, 2.0), (:R3, 1.0), (:R4, nothing))
        ((:R1, 1.0), (:R2, 2.0), (:R3, 2.0), (:R4, nothing))
        ((:R1, 1.0), (:R2, 2.0), (:R3, nothing), (:R4, 1.0))
        ((:R1, 1.0), (:R2, 2.0), (:R3, nothing), (:R4, 2.0))
        ((:R1, 2.0), (:R2, 2.0), (:R3, 1.0), (:R4, nothing))
        ((:R1, 2.0), (:R2, 2.0), (:R3, 2.0), (:R4, nothing))
        ((:R1, 2.0), (:R2, 2.0), (:R3, nothing), (:R4, 1.0))
        ((:R1, 2.0), (:R2, 2.0), (:R3, nothing), (:R4, 2.0))
    ]
    @test length(sweepvars(s)) == 4
    @test :R1 ∈ sweepvars(s)
    @test :R2 ∈ sweepvars(s)
    @test :R3 ∈ sweepvars(s)
    @test :R4 ∈ sweepvars(s)

    # Test that a single value in any of our larger sweep types just returns a `Sweep`:
    @test ProductSweep(r1 = 1:10) == Sweep(r1 = 1:10)
    @test SerialSweep(r1 = 1:10) == Sweep(r1 = 1:10)
    @test TandemSweep(r1 = 1:10) == Sweep(r1 = 1:10)

    # Test that equality works with vectors as well
    @test Sweep(r1 = [1, 2, 3]) == Sweep(r1 = [1, 2, 3])
end

@testset "alter() on NamedTuples" begin
    p = (r=1, cap=1e-12)
    @test alter(p, first(Sweep(cap=1e-13))) == (r=1, cap=1e-13)
    @test alter(p, first(ProductSweep(r=2:3, cap = 1e-13))) == (r=2, cap=1e-13)
end

@testset "split_axes()" begin
    ps = ProductSweep(A = 1:10, B=1:10, C=1:5, D=1:5)

    # Test that `split_axes()` works as it's supposed to
    for vars in [[:A, :C], (:A, :C), Set([:A, :C])]
        outer, inner = split_axes(ps, vars)
        @test length(sweepvars(outer)) == 2
        @test length(sweepvars(inner)) == 2
        @test :B ∈ sweepvars(outer)
        @test :D ∈ sweepvars(outer)
        @test :A ∈ sweepvars(inner)
        @test :C ∈ sweepvars(inner)
        @test size(outer) == (10, 5)
        @test size(inner) == (10, 5)
        @test size(outer, 1) == 10
        @test size(inner, 1) == 10
        @test size(outer, 2) == 5
        @test size(inner, 2) == 5

        # Test that ProductSweep is the inverse operation:
        ps2 = ProductSweep(outer, inner)
        @test sweepvars(ps) == sweepvars(ps2)
        # Although ordering doesn't quite match, due to how we've split things up
        @test size(ps) != size(ps2)

        # Test that trying to split on a non-existant axis errors:
        @test_throws ArgumentError split_axes(ps, [:E])

        # Test that trying to split some other kind of sweep doesn't work:
        @test_throws ArgumentError split_axes(SerialSweep(A=1:10, B=1:10), [:A])
    end

end

@testset "CircuitSweep" begin
    # Test construction of the `CircuitSweep` object
    cs = CircuitSweep(TwoResistorCircuit, Sweep(R1 = 1.0:10.0))
    @test length(cs) == 10
    @test size(cs) == (10,)
    @test size(cs, 1) == 10

    # Show that it generates a circuit struct with the parameters as we expect
    @test first(cs).params.R1 == 1.0
    # TODO: Add a better way to get the "full" parameterization from a ParamSim
    #@test first(cs).params.R2 == 1000.0

    # Test that a two-dimensional sweep is represented two-dimensionally
    cs = CircuitSweep(TwoResistorCircuit, ProductSweep(R1 = 1.0:10.0, R2 = 1.0:10.0))
    @test length(cs) == 100
    @test size(cs) == (10,10)
    @test size(cs, 1) == 10
    @test size(cs, 2) == 10
    @test size(collect(cs)) == (10,10)
    @test first(cs).params.R1 == 1.0
    @test first(cs).params.R2 == 1.0
    @test length(sweepvars(cs)) == 2
    @test :R1 ∈ sweepvars(cs)
    @test :R2 ∈ sweepvars(cs)

    # If we try to construct a `CircuitSweep` that sets an invalid property name,
    # ensure we get an appropriate error:
    #@test_throws KeyError CircuitSweep(TwoResistorCircuit,
    #    ProductSweep(R1 = 100.:100.:2000.,
    #                 R3 = 100.:100.:2000.),
    #)

    # If we try to collect a `CircuitSweep` that sets an invalid property value,
    # ensure we get an appropriate error:
    #@test_throws MethodError collect(CircuitSweep(TwoResistorCircuit, Sweep(R1 = ["a", "b", "c"])))

    # Test that we can use var-strings to reach into deeply-nested types
    struct NestedCircuitParams{A,B}
        a::A
        b::B
    end
    NestedCircuitParams() = NestedCircuitParams(
        NestedCircuitParams(1.0, 2.0),
        NestedCircuitParams(3.0, NestedCircuitParams(4.0, 5.0)),
    )
    function (self::NestedCircuitParams)()
        vcc = Named(net, "vcc")()
        gnd = Named(net, "gnd")()
        Named(V(1.), "V")(vcc, gnd)
        Named(Gnd(), "G")(gnd)
    end
    sweep = ProductSweep(
        TandemSweep(var"a.a" = 1.0:2.0,
                    var"a.b" = 1.0:2.0),
        SerialSweep(var"b.a" = 1.0:2.0,
                    var"b.b.a" = 2.0:3.0),
    )
    @test isa(repr(sweep), String)
    cs = CircuitSweep(NestedCircuitParams, sweep)
    @test length(cs) == 8
    @test size(cs) == (2,4)
    @test size(cs, 1) == 2
    @test size(cs, 2) == 4
    @test first(cs).params.a.a == 1
    @test first(cs).params.a.b == 1
    @test first(cs).params.b.a == 1
    # TODO: wait until we can get the fully concrete result
    #@test first(cs).params.b.b.a == 4
    #@test last(cs).params.b.a == 3
    @test last(collect(cs)).params.b.b.a == 3
    @test length(sweepvars(cs)) == 4
    @test Symbol("a.a") ∈ sweepvars(cs)
    @test Symbol("a.b") ∈ sweepvars(cs)
    @test Symbol("b.a") ∈ sweepvars(cs)
    @test Symbol("b.b.a") ∈ sweepvars(cs)
end

@testset "simple dc!" begin
    dc!(TwoResistorCircuit(100., 100.))
    dc!(ParamSim(TwoResistorCircuit; R1=100., R2=100.))
end

@testset "dc! sweeps" begin
    # Construct `CircuitSweep` object, which can be broadcast over with `dc!`, etc...
    cs = CircuitSweep(TwoResistorCircuit, ProductSweep(R1 = 100.:100.:2000., R2 = 100.:100.:2000.))

    # Perform the solve; `dc!` contains a custom broadcasting override to save work
    # so make sure you use it when possible, otherwise you'll have to manage the
    # `DAEProblem` and other details yourself.
    solutions = dc!(cs; abstol=deftol, reltol=deftol)

    # Note the unpacking of the named tuple from each `s in cs` into `R1` and `R2`.
    for (sol, (;R1, R2)) in zip(solutions, [s.params for s in cs])
        # Ensure that the current is inversely proportional to the overall resistance:
        @test isapprox_deftol(-1/(R1 + R2), sol[cs.sys.V.I])
    end
end

@testset "dc! sweep on spice code" begin
    spice_code =
    """
        * Parameter scoping test

        .subckt subcircuit1 vss gnd
        .param r_load=1
        r1 vss gnd 'r_load'
        .ends

        .param v_in=1
        x1 vss 0 subcircuit1
        v1 vss 0 'v_in'
    """
    circuit_code = CedarSim.make_spectre_circuit(
        CedarSim.SpectreNetlistParser.SPICENetlistParser.SPICENetlistCSTParser.parse(spice_code),
    );
    circuit = eval(circuit_code);
    cs = CircuitSweep(circuit, ProductSweep(v_in = 1.0:10.0, var"x1.r_load" = 1.0:10.0))
    solutions = dc!(cs; abstol=deftol, reltol=deftol)

    for sol in solutions
        params = sol.prob.p.params
        v_in = params.params.v_in
        r_load = params.x1.params.r_load
        # Ensure that the current is V/R:
        # TODO: get `dc!()` to return a scalar here!
        @test isapprox_deftol(v_in/r_load, sol[cs.sys.x1.r1.I][end])
    end
end

@testset "find_param_ranges" begin
    # Create a nasty complicated parameter exploration
    params = ProductSweep(
        ProductSweep(
            SerialSweep(
                # Work around inability to provide duplicate kwarg names)
                Sweep(a = 1:10),
                Sweep(a = 11:20),
            ),
            b = 1:2:5,
        ),
        ProductSweep(
            TandemSweep(
                c = 1:10,
                d = 1:10,
            ),
            SerialSweep(
                c = 11:15,
                d = 1:5,
            )
        ),
    )
    ranges = CedarSim.find_param_ranges(params)
    @test ranges[:a] == (1, 20, 20)
    @test ranges[:b] == (1, 5, 3)
    @test ranges[:c] == (1, 15, 15)
    @test ranges[:d] == (1, 10, 15)
end

@testset "sweepify" begin
    s1 = sweepify([(r1 = 1:10, r2 = 1:6), (r3 = 1:4, r4=1:2)])
    s2 = SerialSweep(ProductSweep(r1 = 1:10, r2 = 1:6), ProductSweep(r3 = 1:4, r4=1:2))
    @test collect(s1) == collect(s2)

    s1 = sweepify([:r1 => 1:10, :r2 => 1:10])
    s2 = SerialSweep(r1 = 1:10, r2 = 1:10)
    @test collect(s1) == collect(s2)
end

end # module test_sweep
