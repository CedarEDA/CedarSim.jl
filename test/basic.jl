module basic_tests

include("common.jl")

@testset "Unimplemented Device" begin
    # If we try to use it, we get an `UnsupportedIRException`, since the
    # generated code contains an `error()` which `DAECompiler` doesn't like.
    function ERRcircuit()
        vcc = Named(net, "vcc")()
        gnd = Named(net, "gnd")()
        Named(CedarSim.UnimplementedDevice(), "U")(vcc, gnd)
    end
    # Trying to directly run `ERRcircuit()` throws an error
    # due to the `error()` in the implementation of `UnimplementedDevice()`
    @test_throws CedarSim.CedarError ERRcircuit()

    # DAECompiler sees the `error()` and complains that it is unsupported IR:
    @test_throws CedarSim.DAECompiler.UnsupportedIRException CircuitIRODESystem(ERRcircuit)
end

@testset "Simple VR Circuit" begin
    function VRcircuit()
        vcc = Named(net, "vcc")()
        gnd = Named(net, "gnd")()
        Named(V(5.), "V")(vcc, gnd)
        Named(R(2), "R")(vcc, gnd)
        Gnd()(gnd)
    end

    debug_config = (;
        ir_log = joinpath(@__DIR__, "ir", "basic", "simple_VR_circuit"),
        store_ir_levels = true,
    )
    sys, sol = solve_circuit(VRcircuit; debug_config)

    # I = V/R = 5/2
    R_i = sol[sys.R.I]
    R_v = sol[sys.R.V]
    @test isapprox_deftol(R_v[1], 5.0)
    @test isapprox_deftol(R_v[end], 5.0)
    @test isapprox_deftol(R_i[1], 2.5)
    @test isapprox_deftol(R_i[end], 2.5)
end

@testset "MC VR Circuit" begin
    # TODO ugly af we need a better api to parameterize SimSpec
    struct MCcircuit
        seed::UInt
    end
    function (ckt::MCcircuit)()
        # currently disabled due to inference issues
        # CedarSim.circuit_sls[].spec = CedarSim.SimSpec(var"$time"(), 300, 0, Random.Xoshiro(ckt.seed))
        vcc = Named(net, "vcc")()
        gnd = Named(net, "gnd")()
        Named(V(5.), "V")(vcc, gnd)
        Named(R(agauss(2, 3, 3)), "R")(vcc, gnd)
        Gnd()(gnd)
    end

    debug_config = (;
        ir_log = joinpath(@__DIR__, "ir", "basic", "MC_VR_circuit1"),
        store_ir_levels = true,
    )
    sys1, sol1 = solve_circuit(MCcircuit(1); debug_config)

    debug_config = (;
        ir_log = joinpath(@__DIR__, "ir", "basic", "MC_VR_circuit2"),
        store_ir_levels = true,
    )
    sys2, sol2 = solve_circuit(MCcircuit(2); debug_config)

    i1 = sol1[sys1.R.I]
    i2 = sol2[sys2.R.I]
    # test that the RNG is consistent between timesteps
    @test allapprox_deftol(i1)
    @test allapprox_deftol(i2)
    # but different between runs
    @test_broken !isapprox_deftol(first(i1), first(i2))
end

@testset "Simple IR circuit" begin
    function IRcircuit()
        icc = Named(net, "icc")()
        gnd = Named(net, "gnd")()
        # Note; we follow the SPICE convention here and use negative current
        # to denote current flowing from the negative to positive terminals
        # of the current source.
        Named(I(-5.0), "I")(icc, gnd)
        Named(R(2), "R")(icc, gnd)
        Gnd()(gnd)
    end

    debug_config = (;
        ir_log = joinpath(@__DIR__, "ir", "basic", "IR_circuit"),
        store_ir_levels = true,
    )
    sys, sol = solve_circuit(IRcircuit; debug_config)

    # V = IR = 5*2
    R_i = sol[sys.R.I]
    R_v = sol[sys.R.V]
    @test isapprox_deftol(R_v[1], 10.0)
    @test isapprox_deftol(R_v[end], 10.0)
    @test isapprox_deftol(R_i[1], 5.0)
    @test isapprox_deftol(R_i[end], 5.0)
end

const v_val = 5.0
const r_val = 2000.0
const c_val = 1e-6
@testset "Simple VRC circuit" begin
    function VRCcircuit()
        vcc = Named(net, "vcc")()
        vrc = Named(net, "vrc")()
        gnd = Named(net, "gnd")()
        Named(V(v_val), "V")(vcc, gnd)
        Named(R(r_val), "R")(vcc, vrc)
        Named(C(c_val), "C")(vrc, gnd)
        Gnd()(gnd)
    end

    debug_config = (;
        ir_log = joinpath(@__DIR__, "ir", "basic", "VRC_circuit"),
        store_ir_levels = true,
    )

    # By passing `[0.0]` in as our `u0`, we explicitly ask for a
    # simulation where the capacitor starts completely uncharged.
    # Otherwise, we start with a small, but random voltage.
    sys, sol = solve_circuit(VRCcircuit; u0=[0.0], debug_config)

    # This RC circuit has a time constant much smaller than that of
    # our simulation time domain, so let's ensure that the beginning
    # and end points of our simulation follow physical laws:
    c_i = sol[sys.C.I]
    @test isapprox_deftol(c_i[1], v_val/r_val)
    @test isapprox_deftol(c_i[end], 0)
    c_v = sol[sys.C.V]
    @test isapprox_deftol(c_v[1], 0)
    @test isapprox_deftol(c_v[end], v_val)
end

using CedarSim: ParallelInstances
@testset "ParallelInstances" begin
    function MultiVRCcircuit()
        vcc = Named(net, "vcc")()
        vrc = Named(net, "vrc")()
        gnd = Named(net, "gnd")()
        Named(V(v_val), "V")(vcc, gnd)
        Named(ParallelInstances(R(r_val), 10), "R")(vcc, vrc)
        Named(C(c_val), "C")(vrc, gnd)
        Gnd()(gnd)
    end

    sys, sol = solve_circuit(MultiVRCcircuit; u0=[0.0])

    # This RC circuit has a time constant much smaller than that of
    # our simulation time domain, so let's ensure that the beginning
    # and end points of our simulation follow physical laws:
    c_i = sol[sys.C.I]
    @test isapprox_deftol(c_i[1], 10*v_val/r_val)
    @test isapprox_deftol(c_i[end], 0)
    c_v = sol[sys.C.V]
    @test isapprox_deftol(c_v[1], 0)
    @test isapprox_deftol(c_v[end], v_val)
end

@testset "Simple Spectre sources" begin
    mktempdir() do dir
        # We write this out to a file so that we can test reading of
        # spectre sources from disk, rather than just an IOBuffer
        spectre_file = joinpath(dir, "sources.scs")
        open(spectre_file; write=true) do io
            write(io, """
            I1 (0 1) isource dc=2.2u
            R1 (1 0) resistor r=1000

            I2 (0 2) isource type=pwl wave=[0 1m .5 2m 1 1.75m]
            R2 (2 0) resistor r=2k

            V3 (0 3) vsource dc=1.5
            R3 (3 0) resistor r=1k

            V4 (0 4) vsource type=pwl wave=[ 0 1 .5 2 \
                    1 5]
            R4 (4 0) resistor r=4k

            B5 (0 5) bsource v=\$time*V(3)
            R5 (5 0) resistor r=1k
            """)
        end

        sys, sol = solve_spectre_file(spectre_file);
        @test all(isapprox.(sol[sys.node_1], 2.2e-3))
        @test all(isapprox.(sol[sys.R1.I], 2.2e-6))
        @test all(isapprox.(sol[sys.node_3], -1.5))
        @test all(isapprox.(sol[sys.R3.I], -1.5e-3))

        @test isapprox(sol[sys.node_2][end], 3.5)
        @test isapprox(sol[sys.R2.I][end], 1.75e-3)
        @test isapprox(sol[sys.node_4][end], -5.)
        @test isapprox(sol[sys.R4.I][end], -1.25e-3)
        @test isapprox(sol[sys.node_5][end], 1.5)
    end
end

@testset "Simple SPICE sources" begin
    spice_code = """
    * Simple SPICE sources
    V1 0 1 1
    R1 1 0 1k

    B5 0 5 v=V(1)*2
    R5 5 0 1k

    E6 0 6 0 5 2
    R6 6 0 r=1k

    E8 0 8 vol=V(0, 5)*2
    R8 8 0 r=1k

    G7 0 7 0 5 2
    R7 7 0 r=1k

    G9 0 9 cur=V(0, 5)*2
    R9 9 0 r=1k
    """

    sys, sol = solve_spice_code(spice_code);
    @test all(isapprox.(sol[sys.node_5], 2.0))
    @test all(isapprox.(sol[sys.node_6], 4.0))
    @test all(isapprox.(sol[sys.node_8], 4.0))
    @test all(isapprox.(sol[sys.node_7], -4000.0))
    @test all(isapprox.(sol[sys.node_9], -4000.0))
end

#=
@testset "Simple SPICE switch" begin
    spice_code = """
    .model switch1 sw vt=0.5 vh=0.1 ron=1 roff=1k
    V1 vcc 0 1
    s1 vcc 0 ctl 0 switch1 ON
    V2 ctl 0 PWL(0.0,0.0,1.0,1.0,2.0,0.0)
    """

    sys, sol = solve_spice_code(spice_code, time_bounds=(0, 2));
    @test isapprox(sol(0.0, idxs=sys.s1.I), 1e-3)
    @test isapprox(sol(0.5, idxs=sys.s1.I), 1e-3)
    @test isapprox(sol(1.0, idxs=sys.s1.I), 1)
    @test isapprox(sol(1.5, idxs=sys.s1.I), 1)
    @test isapprox(sol(2.0, idxs=sys.s1.I), 1e-3)

    spice_code = """
    .model switch1 sw vt=0.5 vh=0.1 ron=1 roff=1k
    V1 vcc 0 1
    s1 vcc 0 ctl 0 switch1 ON
    s2 vcc 0 ctl 0 switch1 OFF
    V2 ctl 0 0.5
    """

    sys, sol = solve_spice_code(spice_code, time_bounds=(0, 2));
    @test all(isapprox.(sol[sys.s1.I], 1.0))
    @test all(isapprox.(sol[sys.s2.I], 1e-3))
end
=#

@testset "Simple Spectre subcircuit" begin
    ckt = """
    subckt myres vcc gnd
        parameters r=1k
        r1 (vcc gnd) resistor r=r
    ends myres

    x1 (vcc 0) myres r=2k
    v1  (vcc 0) vsource dc=1
    """
    sys, sol = solve_spectre_code(ckt);
    @test all(isapprox.(sol[sys.x1.r1.I], 0.5e-3))
end

@testset "SPICE include .LIB" begin
    # Ensure we can use .LIB includes
    mktempdir() do dir
        # We write this out to a file so that we can test reading of
        # spice sources from disk, and self-inclusion.
        spice_file = joinpath(dir, "selfinclude.cir")
        open(spice_file; write=true) do io
            write(io, """
            * .LIB definition and include test
            V1 vdd 0 1

            .LIB my_lib
            r1 vdd 0 1337
            .ENDL
            .LIB "selfinclude.cir" my_lib
            """)
        end

        sys, sol = solve_spice_file(spice_file)
        @test isapprox_deftol(sol[sys.r1.I][end], 1/1337)
    end

    # Ensure we get a good error message if we try to include a non-existent `.lib`:
    mktempdir() do dir
        spice_file = joinpath(dir, "badinclude.cir")
        open(spice_file; write=true) do io
            write(io, """
            * .LIB definition and include test
            .LIB my_lib
            r1 vdd 0 1337
            .ENDL
            .LIB "badinclude.cir" not_my_lib
            """)
        end

        try
            solve_spice_file(spice_file)
            @test false
        catch e
            @test occursin("Unable to find section", e.msg)
        end
    end

    # Test that we can use spice strings and julia includes to load spice files
    mktempdir() do dir
        spice_file = joinpath(dir, "resistor.cir")
        open(spice_file; write=true) do io
            write(io, """
            * Subcircuit 1
            .subckt subcircuit1 vss gnd
                r1 vss gnd 1k
            .ends
            """)
        end

        Base.include(@__MODULE__, SpcFile(spice_file, true))
    end

    # acltually @sp_str can only define things at the top level
    # and splicing a temp folder is also a giant PITA
    # so instead of .lib I'll just make sure @sp_str works
    sp"""
    * Subcircuit 2
    .subckt subcircuit2 vss gnd
        r2 vss gnd 2k
    .ends
    """

    ckt = """
    * Circuit of Subcircuits
    V1 vcc 0 1
    x1 vcc out subcircuit1
    x2 out 0 subcircuit2
    """
    sys, sol = solve_spice_code(ckt);
    @test all(isapprox.(sol[sys.node_out], 2/3))
end

@testset "Verilog include" begin
    ckt = """
    ahdl_include "resistor.va"

    x1 (vcc 0) BasicVAResistor R=2k
    v1 (vcc 0) vsource dc=1
    """
    inc = joinpath(dirname(pathof(CedarSim.VerilogAParser)), "../test/inputs")
    sys, sol = solve_spectre_code(ckt; include_dirs=[inc]);
    @test all(isapprox.(sol[sys.v1.I], -1/2e3))

    ckt = """
    * Verilog Include 2
    .hdl "resistor.va"

    x1 vcc 0 BasicVAResistor r=2k
    v1 vcc 0 dc=1
    """
    inc = joinpath(dirname(pathof(CedarSim.VerilogAParser)), "../test/inputs")
    sys, sol = solve_spice_code(ckt; include_dirs=[inc]);
    @test all(isapprox.(sol[sys.v1.I], -1/2e3))
end

@testset "SPICE parameter scope" begin
    # Ensure we can use parameters scoped to sub-circuits
    # https://github.com/JuliaComputing/CedarSim.jl/pull/136
    spice_code =
    """
    * Parameter scoping test

    .subckt subcircuit1 vss gnd l=11
    .param
    + par_l=1
    + par_leff='l-par_l'
    r1 vss gnd 'par_leff'
    .ends

    x1 vss 0 subcircuit1
    v1 vss 0 1
    """

    sys, sol = solve_spice_code(spice_code);
    @test isapprox_deftol(sol[sys.x1.r1.I][end], 1/10)

    # ensure that we dynamically scope parameters in nested subircuits
    # and test that we can seperately compile models from the netlist
    spice_netlist = """
    * Dynamic parameters
    .subckt inner a b foo=foo+2000
    R1 a b 'foo'
    .ends

    .subckt outer a b
    x1 a b inner
    .ends
    """
    nlast = SpectreNetlistParser.SPICENetlistParser.parse(spice_netlist)
    nlcode = CedarSim.make_spectre_netlist(nlast)
    eval(nlcode)

    spice_ckt = """
    * foo
    .param foo = 1
    i1 vcc 0 'foo'
    x1 vcc 0 outer
    *x1 vcc 0 outer foo=foo+1
    """
    sys, sol = solve_spice_code(spice_ckt);
    @test isapprox_deftol(sol[sys.x1.x1.r1.V][end], -2001.0)

    # Also test Julia interface for subcircuits
    Base.@kwdef struct subcircuit_params
        l::Float64 = 11
        par_l::Float64 = 1
    end

    function (p::subcircuit_params)(vss, gnd)
        Named(R(p.l - p.par_l), "r1")(vss, gnd)
    end

    function subcircuit_circuit()
        vcc = Named(net, "vcc")()
        gnd = Named(net, "gnd")()
        Named(V(1.), "V")(vcc, gnd)
        Named(SubCircuit(subcircuit_params()), "x1")(vcc, gnd)
        Gnd()(gnd)
    end

    debug_config = (;
        ir_log = joinpath(@__DIR__, "ir", "basic", "subcircuit_circuit"),
        store_ir_levels = true,
    )

    sys, sol = solve_circuit(subcircuit_circuit; debug_config)
    @test isapprox_deftol(sol[sys.x1.r1.I][end], 1/10)

    # test that a ckt macro can refer to a model card
    modelspice = """
    * mydio
    .model mydio d
    .subckt mydio p n
    d1 p n mydio
    .ends
    vcc vcc 0 5
    x1 vcc 0 mydio
    """
    mast = SpectreNetlistParser.SPICENetlistParser.parse(modelspice)
    mcode = CedarSim.make_spectre_circuit(mast)
    f = eval(mcode)
    @test f() === nothing

    # test that temper in .param picks up .option temp
    spice_ckt = """
    * param temp
    .option temp=10
    .param foo = temper
    i1 vcc 0 'foo'
    r1 vcc 0 1
    """
    mast = SpectreNetlistParser.SPICENetlistParser.parse(spice_ckt)
    mcode = CedarSim.make_spectre_circuit(mast)
    f = eval(mcode)
    sys, sol = solve_circuit(f);
    @test isapprox_deftol(sol[sys.r1.V][end], -10)

    # same but .temp
    spice_ckt = """
    * .temp
    .temp 10
    .param foo = temper
    i1 vcc 0 'foo'
    r1 vcc 0 1
    """
    mast = SpectreNetlistParser.SPICENetlistParser.parse(spice_ckt)
    mcode = CedarSim.make_spectre_circuit(mast)
    f = eval(mcode)
    sys, sol = solve_circuit(f);
    @test isapprox_deftol(sol[sys.r1.V][end], -10)

    # test overriding temperature
    cir = ParamSim(f, temp=20)
    cir()
    sys, sol = solve_circuit(cir);
    @test isapprox_deftol(sol[sys.r1.V][end], -20)

    # test that the default temper is 27
    spice_ckt = """
    * temper
    .param foo = temper
    i1 vcc 0 'foo'
    r1 vcc 0 1
    """
    sys, sol = solve_spice_code(spice_ckt);
    @test isapprox_deftol(sol[sys.r1.V][end], -27)

    # test that instance parameters can refer to other parameters
    spice_code =
    """
    * Parameter scoping test

    .subckt subcircuit1 vss gnd w=2 rsh=1 nrd=1
    r1 vss gnd 'rsh*nrd'
    .ends
    * this should not require a global w parameter
    x1 vss 0 subcircuit1 w=4 nrd='w/2'
    * currently broken but supported in ngspice
    *x1 vss 0 subcircuit1 nrd='w/2' w=4
    *x1 vss 0 subcircuit1 nrd='w/2'
    v1 vss 0 1
    """

    sys, sol = solve_spice_code(spice_code);
    @test isapprox_deftol(sol[sys.x1.r1.I][end], 1/2)
end

@testset "multimode spice source" begin
    spice = """
    * multimode spice source
    v1 vcc 0 DC 5 AC 1 SIN(10 3 1k)
    r1 vcc 0 1k
    """
    sa = SpectreNetlistParser.parse(IOBuffer(spice); start_lang=:spice)
    code = CedarSim.make_spectre_circuit(sa)
    circuit = eval(code)

    sys = CircuitIRODESystem(circuit);
    prob = DAEProblem(sys, rand(5), rand(5), (0.0, 0.01));
    for (initializealg, vcc_known) in [(CedarDCOp(), 10.0),
                                (ShampineCollocationInit(), 10.0),
                                (CedarTranOp(), 10.0)]

        sol = init(prob, DFBDF(autodiff=false); reltol=deftol, abstol=deftol, initializealg)
        vcc = sol[sys.node_vcc]
        @test isapprox_deftol(vcc, vcc_known) || (initializealg, vcc, vcc_known)
    end
end

@testset "multiplicities" begin
    spice = """
    * multiplicities
    v1 vcc 0 DC 1

    r1a vcc 1 1 m=10
    r1b 1 0 1

    .subckt r10 a b m=10
    r2a a b 1
    .ends
    x2a vcc 2 r10
    r2b 2 0 1

    x3a1 vcc 3 r10 m=5
    x3a2 vcc 3 r10 m=5
    r3b 3 0 1

    .subckt r5t2 a b
    x5r1 a b r10 m=5
    x5r2 a b r10 m=5
    .ends
    x4a1 vcc 4 r5t2
    r4b 4 0 1

    .subckt r2 a b
    r2 a b 1 m=2
    .ends
    x5a vcc 5 r2 m=5
    r5b 5 0 1

    .model rm r R=1
    r6a vcc 6 rm m=10 l=1u
    r6b 6 0 1
    """
    sys, sol = solve_spice_code(spice);
    for net in (sys.node_1, sys.node_2, sys.node_3, sys.node_4, sys.node_5, sys.node_6)
        @test sol[net][end] == sol[net][1] == 10/11
    end
end

@testset ".model case sensitivity" begin
    spice = """
        * .model case sensitivity
        v1 vcc 0 DC 1
        .model rr r R=1
        r1 vcc 1 rr l=1u
        r2 1 0 rr R=2 l=1u
        """
    sys, sol = solve_spice_code(spice);
    @test sol[sys.node_1][end] ≈ 2/3
end

@testset "units and magnitudes" begin
    spice = """
        * units and magnitudes
        i1 vcc 0 DC -1mAmp
        r1 vcc 0 1MegQux
        """
    sys, sol = solve_spice_code(spice);
    @test sol[sys.node_vcc][end] ≈ 1000

    spice = """
        * units and magnitudes 2
        i1 vcc 0 DC -1Amp
        r1 vcc 0 1Mil
        """
    sys, sol = solve_spice_code(spice);
    @test sol[sys.node_vcc][end] ≈ 2.54e-5

    # test that magnitudes don't introduce floating point errors
    spice = """
        * units and magnitudes 3
        .param a=0.22u b=0.22e-6
        """
    mast = SpectreNetlistParser.SPICENetlistParser.parse(spice)
    mcode = CedarSim.make_spectre_circuit(mast)
    f = eval(mcode)
    👀 = CedarSim.ParamObserver()
    f(👀)
    p = 👀.params
    @test p.a === p.b
end

@testset ".option" begin
    spice_ckt = """
    * .option
    .option temp=10 filemode=ascii noinit
    """
    mast = SpectreNetlistParser.SPICENetlistParser.parse(spice_ckt)
    mcode = CedarSim.make_spectre_circuit(mast)
    f = eval(mcode)
    @test f()===nothing
end

@testset "functions" begin
    spice_ckt = """
    * functions
    .param
    + intp=int(1.5)
    + intn=int(-1.5)
    + nintp = nint(1.6)
    + nintn = nint(-1.6)
    + floorp=floor(1.5)
    + floorn=floor(-1.5)
    + ceilp=ceil(1.5)
    + ceiln=ceil(-1.5)
    + powp=pow(2.0, 3)
    + pown=pow(2.0, -3)
    + lnp=ln(2.0)
    """
    mast = SpectreNetlistParser.SPICENetlistParser.parse(spice_ckt)
    mcode = CedarSim.make_spectre_circuit(mast)
    f = eval(mcode)
    👀 = CedarSim.ParamObserver()
    f(👀)
    p = 👀.params
    @test p.intp == 1
    @test p.intn == -1
    @test p.nintp == 2
    @test p.nintn == -2
    @test p.floorp == 1
    @test p.floorn == -2
    @test p.ceilp == 2
    @test p.ceiln == -1
    @test p.powp == 8
    @test p.pown == 0.125
    @test p.lnp == log(2.0)
end

@testset "device == param" begin
    spice = """
        * device == param
        .param x1=1
        .subckt myres p n
            .param rload=1k
            rload p n 'rload*x1'
        .ends
        i1 vcc 0 DC -1
        x1 vcc 0 myres
        """
    mast = SpectreNetlistParser.SPICENetlistParser.parse(spice)
    mcode = CedarSim.make_spectre_circuit(mast)
    f = eval(mcode)
    👀 = CedarSim.ParamObserver(x1=2.0)
    f(👀)
    @test @param(👀.x1.rload)*@param(👀.x1) == @param(👀.x1.rload.r)
    @test CedarSim.canonicalize_params((; params=(;boo=4), foo=2, bar=(; baz=3))) == (params = (boo = 4, foo = 2), bar = (params = (baz = 3,),))
    @test convert(NamedTuple, 👀) == (
        params = (x1 = 2.0,),
        i1 = (dc = -1,),
        m = 1.0,
        x1 = (
            params = (rload = 1000.0,),
            m = 1.0,
            rload = (r = 2000.0,)
        )
    )
    # dense roundtrip actually hits some error
    sim = ParamSim(f; convert(NamedTuple, 👀)...)
    @test_broken solve_circuit(sim)[2].retcode == SciMLBase.ReturnCode.Success
    # but you can definitely specify all the conflicting parameters in a consistent way
    sim = ParamSim(f; params=(;x1=2.0), x1=(;rload=500,))
    sim()
    sys, sol = solve_circuit(sim);
    @test sol[sys.x1.rload.V][end] ≈ 1000

end

@testset "semiconductor resistor" begin
    spice_code = """
    * semiconductor resistor
    .model myres r rsh=500
    .param res=1k
    v1 vcc 0 1
    R1 vcc 0 myres w=1m l=2m
    R2 vcc 0 res
    """
    sys, sol = solve_spice_code(spice_code);
    @test sol[sys.r1.I][end] ≈ 1e-3
    @test sol[sys.r2.I][end] ≈ 1e-3
end

@testset "ifelse" begin
    spice_code = """
    * ifelse resistor
    .param switch=1
    v1 vcc 0 1
    .if (switch == 1)
    R1 vcc 0 1
    .else
    R1 vcc 0 2
    .endif
    """
    sys, sol = solve_spice_code(spice_code);
    @test sol[sys.r1.I][end] ≈ 1.
end

end # basic_tests
