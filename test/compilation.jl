module compilation_tests

include("common.jl")

@testset "Non-const circuit elements break compilation" begin
    # If we try to use a mutable binding to e.g. SimpleResistor,
    # DAECompiler is unable to prove it will not change mid-simulation,
    # and this breaks our static assumptions.  Assert this, so that
    # if things change, we know to update documentation/coding style guides.
    global my_R = CedarSim.SimpleResistor
    my_V(v) = CedarSim.VoltageSource(dc=v)
    function VRcircuit()
        vcc = Named(net, "vcc")()
        gnd = Named(net, "gnd")()
        Named(my_V(5.), "V")(vcc, gnd)
        Named(my_R(2), "R")(vcc, gnd)
        Gnd()(gnd)
    end

    @test CircuitIRODESystem(VRcircuit) !== nothing
end

@testset "Non-const values cause parameterization error" begin
    # If we define non-const values in a circuit such as the following,
    # we get a `DAECompiler` error when trying to solve it.
    L1_val = 3/2
    C2_val = 4/3
    L3_val = 1/2
    R4_val = 1.0
    ω_val = 1.0
    function butterworth_circuit()
        vin = Named(net, "vin")()
        n1 = Named(net, "n1")()
        vout = Named(net, "vout")()
        gnd = Named(net, "gnd")()

        Named(V(sin(ω_val*sim_time())), "V")(vin, gnd)
        Named(L(L1_val), "L1")(vin, n1)
        Named(C(C2_val), "C2")(n1, gnd)
        Named(L(L3_val), "L3")(n1, vout)
        Named(R(R4_val), "R4")(vout, gnd)
        Gnd()(gnd)
    end

    try
        debug_config = (;
            ir_log = joinpath(@__DIR__, "ir", "compilation", "butterworth"),
            store_ir_levels = true,
        )
        solve_circuit(butterworth_circuit; debug_config)
        # This should fail
        @test_broken false
    catch e
        @test startswith(e.msg, "Specifying parameterization is required for non-singleton model")
    end
end

@testset "Subcircuit net naming conflict" begin
    spice_ckt = """
    * Create internal `d` net to conflict with external `d` net,
    * which will actually get tied to ground here, causing `i1`
    * to be floating and have zero current.
    .subckt inner d g s
    r1 d g 1
    r2 d s 2
    .ends

    i1 0 d '1'
    x1 0 d 0 inner
    """

    ast = SpectreNetlistParser.SPICENetlistParser.parse(spice_ckt)
    code = CedarSim.make_spectre_circuit(ast)
    circuit = eval(code)

    sim = CedarSim.ParamSim(circuit; x1=(x1=(foo=2.0,),))
    sys, sol = solve_circuit(sim);
    @test isapprox_deftol(sol[sys.i1.I][end], 1.0)
end

end # module compilation_tests
