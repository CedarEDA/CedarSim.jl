using CedarSim
using Test
using CedarSim.SpectreEnvironment
using VerilogAParser
using SpectreNetlistParser
using DAECompiler
using DescriptorSystems
using ControlSystemsBase
using RobustAndOptimalControl

const L1_val = 3/2
const C2_val = 4/3
const L3_val = 1/2
const R4_val = 1
const ω_val = 1

spice_code = """
*Third order low pass filter, butterworth, with ω_c = 1
.param res=$(R4_val)

V1 vin 0 AC 1 SIN (0, 1, $(ω_val/2π))
L1 vin n1 $(L1_val)
C2 n1 0 $(C2_val)
L3 n1 vout $(L3_val)
* conceptually one resistor, split in two make a less trivial noise dss
R4 vout 0 '2*res'
R5 vout 0 '2*res'
"""

ast = CedarSim.SpectreNetlistParser.SPICENetlistParser.SPICENetlistCSTParser.parse(spice_code);
circuit_code = CedarSim.make_spectre_circuit(ast);
circuit = eval(circuit_code);
circuit()

circ = CedarSim.ParamSim(circuit; res=R4_val, temp=23, gmin=0, foo=1)
ac = ac!(circ; reltol=1e-6, abstol=1e-6)

ωs = 2π .* acdec(20, 0.01, 10) # equivalent to spice .ac dec 10 0.01 10
sys = DAECompiler.IRODESystem(ac)
resp_sim = DescriptorSystems.freqresp(ac, sys.node_vout, ωs) # compute frequency response

# analytic
s = tf("s")
H = 1/((s+1)*(s^2+s+1))
resp_an = ControlSystemsBase.freqrespv(H, ωs)

@test resp_sim ≈ resp_an

@test all(DescriptorSystems.freqresp(ac, sys.node_vin, ωs) .≈ 1.0) # check directly-observed source

# convert to ControlSystems state space, compute bode
# (also works for bodeplot and other functions)
mag_sim, phase_sim, w_sim = bode(ss(ac[sys.node_vout]), ωs)
mag_an, phase_an, w_an = bode(H, ωs)

@test mag_sim ≈ mag_an
@test phase_sim ≈ phase_an
@test w_sim ≈ w_an

# test obserables
obs = DescriptorSystems.freqresp(ac, sys.l3.V, ωs)
G = s*L3_val*H
an = ControlSystemsBase.freqrespv(G, ωs)
@test obs ≈ an


## AC noise

noise = noise!(circ; reltol=1e-6, abstol=1e-6);
sys = DAECompiler.IRODESystem(noise)
psd = sqrt.(abs.(CedarSim.PSD(noise, sys.node_vout, ωs)))

k = 1.380649e-23
T = 23+273.15
# I messed up my own derivation somewhere
# H = (s^3*L1_val*L3_val*C2_val*R4_val + s*R4_val*(L1_val+L3_val))/(s^3*L1_val*L3_val*C2_val + s^2*R4_val*L1_val*C2_val + s(L1_val+L3_val))
# so let's just automate it
par(a, b) = a*b/(a+b)
H = minreal(par(par(s*L1_val, 1/(s*C2_val))+s*L3_val, R4_val))
Hn = (4*k*T/R4_val)*H^2
apsd = sqrt.(abs.(ControlSystemsBase.freqrespv(Hn, ωs)))

ngspice = [
1.000000e-02    1.603907e-11
1.122018e-02    1.798692e-11
1.258925e-02    2.016863e-11
1.412538e-02    2.261119e-11
1.584893e-02    2.534419e-11
1.778279e-02    2.839994e-11
1.995262e-02    3.181340e-11
2.238721e-02    3.562195e-11
2.511886e-02    3.986493e-11
2.818383e-02    4.458278e-11
3.162278e-02    4.981562e-11
3.548134e-02    5.560092e-11
3.981072e-02    6.196984e-11
4.466836e-02    6.894148e-11
5.011872e-02    7.651384e-11
5.623413e-02    8.464925e-11
6.309573e-02    9.325069e-11
7.079458e-02    1.021222e-10
7.943282e-02    1.109022e-10
8.912509e-02    1.189531e-10
1.000000e-01    1.251915e-10
1.122018e-01    1.278853e-10
1.258925e-01    1.245993e-10
1.412538e-01    1.127739e-10
1.584893e-01    9.137246e-11
1.778279e-01    6.257326e-11
1.995262e-01    3.108018e-11
2.238721e-01    1.301234e-12
2.511886e-01    2.442666e-11
2.818383e-01    4.559016e-11
3.162278e-01    6.259225e-11
3.548134e-01    7.611463e-11
3.981072e-01    8.683148e-11
4.466836e-01    9.531869e-11
5.011872e-01    1.020422e-10
5.623413e-01    1.073718e-10
6.309573e-01    1.115988e-10
7.079458e-01    1.149529e-10
7.943282e-01    1.176152e-10
8.912509e-01    1.197290e-10
1.000000e+00    1.214075e-10
1.122018e+00    1.227405e-10
1.258925e+00    1.237992e-10
1.412538e+00    1.246401e-10
1.584893e+00    1.253080e-10
1.778279e+00    1.258385e-10
1.995262e+00    1.262599e-10
2.238721e+00    1.265946e-10
2.511886e+00    1.268605e-10
2.818383e+00    1.270717e-10
3.162278e+00    1.272394e-10
3.548134e+00    1.273727e-10
3.981072e+00    1.274785e-10
4.466836e+00    1.275626e-10
5.011872e+00    1.276294e-10
5.623413e+00    1.276824e-10
6.309573e+00    1.277246e-10
7.079458e+00    1.277580e-10
7.943282e+00    1.277846e-10
8.912509e+00    1.278057e-10
1.000000e+01    1.278225e-10
]

# compare ngspice result to analytical solution
@test isapprox(apsd, ngspice[:,2], rtol=1e-6)
@test isapprox(apsd, psd, rtol=1e-6)
# plot(ngspice[:, 1], ngspice[:,2], xscale=:log10, yscale=:log10)
# plot!(ωs/2π, apsd)
# plot!(ωs/2π, psd)

# Nonlinear circuit

if !@isdefined(bsimcmg_inverter)
    @warn "This test is expected to run after the bsimcmg_inverter example."
    include(joinpath(@__DIR__, "bsimcmg", "inverter.jl"))
end

ac = ac!(bsimcmg_inverter.circuit; reltol=1e-6, abstol=1e-6);
sys = DAECompiler.IRODESystem(ac)
# bodeplot(ss(ac[sys.node_q]))

noise = noise!(bsimcmg_inverter.circuit; reltol=1e-6, abstol=1e-6);
sys = DAECompiler.IRODESystem(noise)
ωs = 2π .* acdec(5, 1e3, 1e15)
psd = CedarSim.PSD(noise, sys.node_q, ωs)

# ngspice

ngspice = [
 1.00000000e+03  3.87294780e-06 
 1.58489319e+03  3.07654057e-06 
 2.51188643e+03  2.44397032e-06 
 3.98107171e+03  1.94155038e-06 
 6.30957344e+03  1.54252504e-06 
 1.00000000e+04  1.22564468e-06 
 1.58489319e+04  9.74034192e-07 
 2.51188643e+04  7.74294204e-07 
 3.98107171e+04  6.15787472e-07 
 6.30957344e+04  4.90072210e-07 
 1.00000000e+05  3.90452187e-07 
 1.58489319e+05  3.11619451e-07 
 2.51188643e+05  2.49370353e-07 
 3.98107171e+05  2.00379247e-07 
 6.30957344e+05  1.62016970e-07 
 1.00000000e+06  1.32203300e-07 
 1.58489319e+06  1.09284444e-07 
 2.51188643e+06  9.19292053e-08 
 3.98107171e+06  7.90417976e-08 
 6.30957344e+06  6.96948446e-08 
 1.00000000e+07  6.30887688e-08 
 1.58489319e+07  5.85382820e-08 
 2.51188643e+07  5.54753941e-08 
 3.98107171e+07  5.34526145e-08 
 6.30957344e+07  5.21359610e-08 
 1.00000000e+08  5.12878179e-08 
 1.58489319e+08  5.07453771e-08 
 2.51188643e+08  5.04001025e-08 
 3.98107171e+08  5.01809920e-08 
 6.30957344e+08  5.00421622e-08 
 1.00000000e+09  4.99541499e-08 
 1.58489319e+09  4.98979903e-08 
 2.51188643e+09  4.98611477e-08 
 3.98107171e+09  4.98344328e-08 
 6.30957344e+09  4.98088963e-08 
 1.00000000e+10  4.97710242e-08 
 1.58489319e+10  4.96926794e-08 
 2.51188643e+10  4.95077419e-08 
 3.98107171e+10  4.90584295e-08 
 6.30957344e+10  4.79853276e-08 
 1.00000000e+11  4.55745623e-08 
 1.58489319e+11  4.08285034e-08 
 2.51188643e+11  3.33819587e-08 
 3.98107171e+11  2.46584622e-08 
 6.30957344e+11  1.68510849e-08 
 1.00000000e+12  1.10186810e-08 
 1.58489319e+12  7.05704014e-09 
 2.51188643e+12  4.47983982e-09 
 3.98107171e+12  2.83349593e-09 
 6.30957344e+12  1.78956128e-09 
 1.00000000e+13  1.12957985e-09 
 1.58489319e+13  7.12833688e-10 
 2.51188643e+13  4.49805985e-10 
 3.98107171e+13  2.83832199e-10 
 6.30957344e+13  1.79114468e-10 
 1.00000000e+14  1.13056352e-10 
 1.58489319e+14  7.14008829e-11 
 2.51188643e+14  4.51570112e-11 
 3.98107171e+14  2.86595787e-11 
 6.30957344e+14  1.83456397e-11 
 1.00000000e+15  1.19815076e-11 
]
# plot(ngspice[:, 1], ngspice[:,2], xscale=:log10, yscale=:log10)
# plot!(ωs/2π, sqrt.(abs.(psd)))
@test isapprox(sqrt.(abs.(psd)), ngspice[:,2], rtol=1e-6)
