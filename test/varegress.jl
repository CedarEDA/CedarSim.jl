module varegress

using CedarSim
using DAECompiler
using Sundials, OrdinaryDiffEq
using Test

const C = CedarSim.SimpleCapacitor
const V(v) = CedarSim.VoltageSource(dc=v)

va"""
// Simple Verilog-A Resistor for smoke testing
`include "disciplines.vams"
module VAR(p,n);
    parameter real R = 1000.0 from (0.0:inf] ;
    electrical p, n;
    analog I(p,n) <+ V(p,n)/R;
endmodule
"""

function circuit()
    vcc = Named(net, "vcc")()
    gnd = Named(net, "gnd")()
    out = Named(net, "out")()
    Named(V(1.), "V")(vcc, gnd)
    Named(VAR(1000.), "R")(vcc, out)
    Named(C(1e-9), "C")(out, gnd)
    Named(Gnd(), "G")(gnd)
end

sys1 = CircuitIRODESystem(circuit);
prob_dae1 = DAEProblem(sys1, [0.0], [0.0], (0.0, 1e-5));
prob_ode1 = ODEProblem(sys1, [0.0], (0.0, 1e-5));

sol_dae1 = solve(prob_dae1, IDA())
sol_ode1 = solve(prob_ode1, FBDF(autodiff=false))

@test all(sol_dae1[sys1.R.var"I(p, n)"] .>= 0.)

va"""
// Simple Verilog-A Resistor for smoke testing
`include "disciplines.vams"
module VAR_rev(p,n);
    parameter real R = 1000.0 from (0.0:inf] ;
    electrical p, n;
    analog I(n,p) <+ V(n,p)/R;
endmodule
"""

function circuit2()
    vcc = Named(net, "vcc")()
    gnd = Named(net, "gnd")()
    out = Named(net, "out")()
    Named(V(1.), "V")(vcc, gnd)
    Named(VAR_rev(1000.), "R")(vcc, out)
    Named(C(1e-9), "C")(out, gnd)
    Named(Gnd(), "G")(gnd)
end

sys1 = CircuitIRODESystem(circuit2);
prob_dae1 = DAEProblem(sys1, [0.0], [0.0], (0.0, 1e-5));
prob_ode1 = ODEProblem(sys1, [0.0], (0.0, 1e-5));

sol_dae1 = solve(prob_dae1, IDA())
sol_ode1 = solve(prob_ode1, FBDF(autodiff=false))

@test all(sol_dae1[sys1.R.var"I(p, n)"] .>= 0.)

end
