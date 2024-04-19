module inverter_tests

using CedarSim
using GF180MCUPDK
using BSIM4
using CedarSim.SpectreEnvironment
using SpectreNetlistParser
using OrdinaryDiffEq
using OrdinaryDiffEq.LineSearches
using SciMLBase
using Test
using BenchmarkTools

include(joinpath(Base.pkgdir(CedarSim), "test", "common.jl"))


function do_inverter_test(spice_code)
    # We must use DAEProblem here for now, as there is some issue with ODEProblem, where we get
    # dtmin errors quite regularly, possibly due to numerical precision issues with the extra
    # derivative state.  We hope that Diffractor Jacobians will fix this.
    mod = Module()
    Core.eval(mod, :(using CedarSim))
    Core.eval(mod, :(using CedarSim.SpectreEnvironment))
    circuit_code = CedarSim.make_spectre_circuit(
        CedarSim.SpectreNetlistParser.SPICENetlistParser.SPICENetlistCSTParser.parse(spice_code),
        [Base.pkgdir(CedarSim)],
    );
    circuit = Core.eval(mod, circuit_code);
    invokelatest(circuit)
    sim = DefaultSim(circuit);
    debug_config = (;
        ir_log = joinpath(@__DIR__, "ir", "inverter"),
        store_ir_levels = true,
    )
    sys = CircuitIRODESystem(sim; debug_config);
    prob = DAEProblem(sys, nothing, nothing, (0.0, 4e-7), sim; initializealg=CedarDCOp(;abstol=1e-14), jac=true)
    sol = solve(prob, IDA(); reltol=deftol, abstol=deftol, initializealg=CedarDCOp(;abstol=1e-14))
    @test sol.retcode == ReturnCode.Success
    # Test a few known time points
    @test isapprox_deftol(sol(0.5e-7, idxs=sys.node_d), 0.0)
    @test isapprox_deftol(sol(1.5e-7, idxs=sys.node_d), 5.0)
    @test isapprox_deftol(sol(2.5e-7, idxs=sys.node_d), 0.0)
    @test isapprox_deftol(sol(3.5e-7, idxs=sys.node_d), 5.0)

    # Use larger tolerance here than normal, as there are some leakage currents
    # that pull down the voltage that we're not modeling.
    @test isapprox(sol(0.5e-7, idxs=sys.node_q), 5.0; atol=1e-7, rtol=1e-7)
    @test isapprox(sol(1.5e-7, idxs=sys.node_q), 0.0; atol=1e-7, rtol=1e-7)
    @test isapprox(sol(2.5e-7, idxs=sys.node_q), 5.0; atol=1e-7, rtol=1e-7)
    @test isapprox(sol(3.5e-7, idxs=sys.node_q), 0.0; atol=1e-7, rtol=1e-7)

    # Check no allocations in tgrad
    @test 0 == @ballocated (()->$(prob.f.f.goldclass)($(zeros(length(prob.u0))), $(prob.du0),  $(prob.u0), $sim, 1e-7))()
    # we don't quite get jac to zero because we don't run the optimizer fully.
    @test 5000 > @ballocated (()->$(prob.f.jac.goldclass)($(zeros(length(prob.u0), length(prob.u0))), $(prob.du0),  $(prob.u0), $sim, 0.5,  1e-7))()
end

do_inverter_test("""
* Inverter test

Xneg VSS D Q VSS nfet_06v0 W=3.6e-07 L=6e-07
Xpos VDD D Q VDD pfet_06v0 W=4.95e-07 L=5e-07

VVDD VDD 0 5.0
VVSS VSS 0 0.0
CQ D 0 1e-15
VD D 0 PWL(
+ 000.0e-9 0.0
+ 100.0e-9 0.0
+ 110.0e-9 5.0
+ 200.0e-9 5.0
+ 210.0e-9 0.0
+ 300.0e-9 0.0
+ 310.0e-9 5.0
+ 400.0e-9 5.0
+ )

.LIB "jlpkg://GF180MCUPDK/sm141064.ngspice" typical

.END
""")

# Now do the same but with pulse:
do_inverter_test("""
* Inverter test with pulse

Xneg VSS D Q VSS nfet_06v0 W=3.6e-07 L=6e-07
Xpos VDD D Q VDD pfet_06v0 W=4.95e-07 L=5e-07

VVDD VDD 0 5.0
VVSS VSS 0 0.0
CQ D 0 1e-15
VD D 0 PULSE(0.0 5.0 100ns 10ns 10ns 100ns 200ns)

.LIB "jlpkg://GF180MCUPDK/sm141064.ngspice" typical

.END
""")
end # module
