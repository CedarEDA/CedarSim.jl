module test_spectre

using CedarSim
using SpectreNetlistParser
using SpectreNetlistParser.SPICENetlistParser: SPICENetlistCSTParser
using Test
using CedarSim.SpectreEnvironment


code = """
parameters p1=23pf p2=.3 p3 = 1&2~^3 p4 = true && false || true p5 = M_1_PI * 3.0
r1 (1 0) resistor r=p1      // another simple expression // fdsfdsf
r2 (1 0) resistor r=p2*p2   // a binary multiply expression
r3 (1 0) resistor r=(p1+p2)/p3      // a more complex expression
r4 (1 0) resistor r=sqrt(p1+p2)     // an algebraic function call
r5 (1 0) resistor r=3+atan(p1/p2) //a trigonometric function call
r6 (1 0) resistor r=((p1<1) ? p4+1 : p3)  // the ternary operator
"""

@testset "spectre parameters" begin
ast = SpectreNetlistParser.parse(code);
fn = CedarSim.make_spectre_netlist(ast)
eval(fn)

global_to_julia = CedarSim.SpcScope()
val = ast.stmts[1].params[1].val
@test global_to_julia(val) ≈ 23e-12
val = ast.stmts[1].params[2].val
@test global_to_julia(val) == 0.3
val = ast.stmts[1].params[3].val
@test eval(global_to_julia(val)) == ~((1&2) ⊻ 3)
val = ast.stmts[1].params[4].val
@test eval(global_to_julia(val)) == (true && false || true)
val = ast.stmts[1].params[5].val
@test eval(global_to_julia(val)) == M_1_PI * 3.0

@test p1 ≈ 23e-12
@test p2 == 0.3
@test p3 == ~((1&2) ⊻ 3)
@test p4 == (true && false || true)
@test p5 == M_1_PI * 3.0
end

@testset "3 port BJT" begin
    str = """
    * 3 port BJT
    q0 c b e  vpnp_0p42x10  dtemp=dtemp
    """
    stmt = SPICENetlistCSTParser.parse(IOBuffer(str))
    spc = CedarSim.SpcScope()
    @test spc(stmt.stmts[2]) isa Expr
end

end # module test_spectre
