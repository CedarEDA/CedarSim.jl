module transient_tests

# Include all our testing packages, helper routines, etc...
using CedarSim, PlotlyLight, Cobweb
include(joinpath(Base.pkgdir(CedarSim), "test", "common.jl"))

# We'll create a piecewise linear current source that goes through a resistor
#
# The circuit diagram is:
#
#  ┌──┬── +
#  I  R
#  └──┴── -

const i_max = 2
const r_val = 2
@testset "PWL" begin
    # Helper function that creates the piecewise linear ramp
    # from 0 -> 1 over the course of 1ms -> 9ms
    function pwl_val(t)
        if t < 1e-3
            0
        elseif t > 9e-3
            1
        else
            (t-1e-3)/8e-3
        end
    end

    # The analytic solution of this circuit is easily calculated in terms of `pwl_val(t)`
    vout_analytic_sol(t) = pwl_val(t) * i_max * r_val

    spice_code =
    """
    * PWL test
    .param pval=-1
    i1 vout 0 PWL(1m 0 9m 'pval*$(i_max)')
    R1 vout 0 r=$(r_val)
    """

    # Solve for 10ms instead of the typical full second
    sys, sol = solve_spice_code(spice_code; time_bounds=(0.0, 10e-3));
    @test isapprox_deftol(sol[sys.node_vout], vout_analytic_sol.(sol.t))

    # Solve this circuit again, but this time building it in straight Julia code:
    function PWLIRcircuit()
        vout = Named(net, "vout")()
        gnd = Named(net, "gnd")()
        # Note; we follow the SPICE convention here and use negative current
        # to denote current flowing from the negative to positive terminals
        # of the current source.
        Named(I(pwl_val(sim_time())*i_max*-1), "I")(vout, gnd)
        Named(R(r_val), "R")(vout, gnd)
        Gnd()(gnd)
    end

    debug_config = (;
        ir_log = joinpath(@__DIR__, "ir", "transients", "PWL"),
        store_ir_levels = true,
    )
    sys, sol = solve_circuit(PWLIRcircuit; time_bounds=(0.0, 10e-3), debug_config);
    @test isapprox_deftol(sol[sys.vout], vout_analytic_sol.(sol.t))
end

using StaticArrays, DAECompiler, Diffractor
@testset "PWL derivative" begin
    ts = @SArray[
        000.0e-9,
        100.0e-9,
        110.0e-9,
        200.0e-9,
        210.0e-9,
    ]
    ys = @SArray[
        0.0,
        0.0,
        5.0,
        5.0,
        0.0,
    ]
    example_pwl(t) = CedarSim.pwl_at_time(ts, ys, t)
    var"'" = Diffractor.PrimeDerivativeFwd

    # Test that the derivative at 0 -> 100ns is zero:
    @test isapprox_deftol(example_pwl'(0.0), 0.0)
    @test isapprox_deftol(example_pwl'(50.0e-9), 0.0)
    @test isapprox_deftol(example_pwl'(99.0e-9), 0.0)

    # The discontinuity point itself belongs to the next time period,
    # so its derivative is 5/10e-9:
    @test isapprox_deftol(example_pwl'(100.0e-9), 5.0e8)

    # Same with the next discontinuity
    @test isapprox_deftol(example_pwl'(110.0e-9), 0.0)
    @test isapprox_deftol(example_pwl'(200.0e-9), -5.0e8)
end


# Create a third-order Butterworth filter, according to https://en.wikipedia.org/wiki/Butterworth_filter#Example
# The circuit diagram is:
#
#  ┌─L1─┬─L3─┬── +
#  V    C2   R4
#  └────┴────┴── -
#
# We take the simple example, with values:
#  L1 = 3/2 H
#  C2 = 4/3 F
#  L3 = 1/2 H
#  R4 = 1 Ω
#
# This yields a transfer function of:
#   H(s) = 1/(1 + 2s + 2s^2 + s^3)
# The magnitude of the steady-state response is:
#   G(ω) = 1/sqrt(1 + ω^6)
# so at ω=1 we should get 1/2 gain (note, ω is supplied in radians, so the actual value
# will be divided by 2π!)
#
# If we drive this system with a sinusoidal input with frequency 1, we get the following transfer function:
#   H(s) = 1/(s^2 + 1) * 1/(1 + 2s + 2s^2 + s^3)
# This corresponds to a time-domain solution via the inverse laplace transform of:
#   vout(t) = (e^(-t) - sin(t) - cos(t))/2 + (2 * sin((sqrt(3) * t)/2))/(sqrt(3) * sqrt(e^t))
const L1_val = 3/2
const C2_val = 4/3
const L3_val = 1/2
const R4_val = 1
const ω_val = 1

@testset "Butterworth Filter" begin
    # Helper functino to calculate RMS of a signal
    rms(sig) = sqrt(sum(sig.^2)/length(sig))

    vout_analytic_sol(t) = (exp(-t) - sin(t) - cos(t))/2 + (2 * sin((sqrt(3) * t)/2))/(sqrt(3) * sqrt(exp(t)))
    spice_code = """
    *Third order low pass filter, butterworth, with ω_c = 1

    V1 vin 0 SIN (0, 1, $(ω_val/2π))
    L1 vin n1 $(L1_val)
    C2 n1 0 $(C2_val)
    L3 n1 vout $(L3_val)
    R4 vout 0 $(R4_val)
    """

    # This is a very low-frequency circuit; simulate for a long enough time
    # that we can get a nice steady-state response in the end:
    sys, sol = solve_spice_code(spice_code; time_bounds=(0.0, 100.0), u0 = [0.0, 0.0, 0.0]);
    @test isapprox_deftol(sol[sys.node_vout], vout_analytic_sol.(sol.t))

    # Also assert that the RMS of the steady-state portion is approximately correct:
    @test isapprox(rms(sol[sys.node_vout][end-div(end,2):end]), 0.5, atol=1e-1, rtol=1e-1)

    # Solve this circuit again, but this time building it in straight Julia code:
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

    debug_config = (;
        ir_log = joinpath(@__DIR__, "ir", "transients", "butterworth"),
        store_ir_levels = true,
    )
    sys, sol = solve_circuit(butterworth_circuit; time_bounds=(0.0, 100.0), u0 = [0.0, 0.0, 0.0], debug_config);
    @test isapprox_deftol(sol[sys.vout], vout_analytic_sol.(sol.t))
    @test isapprox(rms(sol[sys.vout][end-div(end,2):end]), 0.5, atol=1e-1, rtol=1e-1)

    # Save out an .html page of the solution, for later analysis
    plots_dir = joinpath(Base.pkgdir(CedarSim), "test", "plots")
    mkpath(plots_dir)
    Cobweb.save(sol, joinpath(plots_dir, "butterworth.html"); title="Butterworth Filter")
end

end # module transient_tests
