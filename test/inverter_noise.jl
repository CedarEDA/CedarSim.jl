module inverter_noise_tests

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

spice_code = """
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
"""

mod = Module()
Core.eval(mod, :(using CedarSim))
Core.eval(mod, :(using CedarSim.SpectreEnvironment))
circuit_code = CedarSim.make_spectre_circuit(
    CedarSim.SpectreNetlistParser.SPICENetlistParser.SPICENetlistCSTParser.parse(spice_code),
    [Base.pkgdir(CedarSim)],
);
circuit = Core.eval(mod, circuit_code);

sim = DefaultSim(circuit);
sys = CircuitIRODESystem(sim);
prob = DAEProblem(sys, nothing, nothing, (0.0, 4e-7), sim; initializealg=CedarDCOp(;abstol=1e-14), jac=true)
sol = solve(prob, IDA(); reltol=deftol, abstol=deftol, initializealg=CedarDCOp(;abstol=1e-14))
@test sol.retcode == ReturnCode.Success

nsol = noise!(circuit)

ngspice = [
1.000000e+03    1.575931e-08
1.584893e+03    1.575931e-08
2.511886e+03    1.575931e-08
3.981072e+03    1.575931e-08
6.309573e+03    1.575931e-08
1.000000e+04    1.575931e-08
1.584893e+04    1.575931e-08
2.511886e+04    1.575931e-08
3.981072e+04    1.575931e-08
6.309573e+04    1.575931e-08
1.000000e+05    1.575931e-08
1.584893e+05    1.575931e-08
2.511886e+05    1.575931e-08
3.981072e+05    1.575931e-08
6.309573e+05    1.575931e-08
1.000000e+06    1.575931e-08
1.584893e+06    1.575931e-08
2.511886e+06    1.575931e-08
3.981072e+06    1.575931e-08
6.309573e+06    1.575931e-08
1.000000e+07    1.575930e-08
1.584893e+07    1.575929e-08
2.511886e+07    1.575927e-08
3.981072e+07    1.575921e-08
6.309573e+07    1.575907e-08
1.000000e+08    1.575870e-08
1.584893e+08    1.575778e-08
2.511886e+08    1.575548e-08
3.981072e+08    1.574968e-08
6.309573e+08    1.573517e-08
1.000000e+09    1.569887e-08
1.584893e+09    1.560881e-08
2.511886e+09    1.538923e-08
3.981072e+09    1.487612e-08
6.309573e+09    1.378417e-08
1.000000e+10    1.184063e-08
1.584893e+10    9.194535e-09
2.511886e+10    6.506000e-09
3.981072e+10    4.333280e-09
6.309573e+10    2.798533e-09
1.000000e+11    1.782755e-09
1.584893e+11    1.129199e-09
2.511886e+11    7.135800e-10
3.981072e+11    4.505166e-10
6.309573e+11    2.843267e-10
1.000000e+12    1.794156e-10
1.584893e+12    1.132080e-10
2.511886e+12    7.143053e-11
3.981072e+12    4.506990e-11
6.309573e+12    2.843725e-11
1.000000e+13    1.794271e-11
1.584893e+13    1.132109e-11
2.511886e+13    7.143126e-12
3.981072e+13    4.507008e-12
6.309573e+13    2.843730e-12
1.000000e+14    1.794272e-12
1.584893e+14    1.132109e-12
2.511886e+14    7.143127e-13
3.981072e+14    4.507008e-13
6.309573e+14    2.843730e-13
1.000000e+15    1.794272e-13
]

ωs = 2π .* acdec(5, 1e3, 1e15)
psd = CedarSim.PSD(nsol, sys.node_q, ωs)
@test isapprox(sqrt.(abs.(psd)), ngspice[:,2], rtol=1e-6)
end # module
