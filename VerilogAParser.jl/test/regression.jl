using VerilogAParser
using VerilogAParser.VerilogACSTParser: Node, VerilogSource
using Test

# Operator precedence issue causing incorrect computation in BSIM-CMG
var = """
module FooBar(p, n); // N.B.: Whitespace at the beginning of this line is part of the test

inout p, n;
electrical p, n;
parameter real R=1 exclude 0;

analog begin
    I(p,n) <+ 1.0 - 2.0*2.0 + 3.0*3.0;
end
endmodule
"""

let res = VerilogAParser.parse(var)
    @test isa(res, Node{VerilogSource})
    mod = res.stmts[1]
    ablock = mod.items[end].item
    astmt = ablock.stmt.stmt.stmts[end].stmt
    rhs = astmt.assign_expr
    # Before the bug fix we would parse this as
    # (- 1.0 (+ (* 2.0 2.0) (* 3.0 3.0))) rather than
    # (+ (- 1.0 (* 2.0 2.0)) (* 3.0 3.0))
    #
    # Test that the toplevel operator is `+`
    @test rhs.op.op == VerilogAParser.VerilogATokenize.Tokens.PLUS
    # While we're at it, test that strings work.
    @test String(rhs.op) == "+"
end

# Formal argument expansion of first token in a macro (https://github.com/CedarEDA/CedarSim.jl/issues/6)
let va = VerilogAParser.parse("""
        `define DefACparam(param_i,param_dc,param_ac) \
           param_i =  (param_dc); \
           if (\$param_given(param_ac) == 1) \
               param_i =  (param_ac);

        module test(x);
               parameter real PLCF=1, PLCFAC=1;
               real PLCFAC_i;
               analog begin
                       `DefACparam(PLCFAC_i,       PLCF,       PLCFAC)
               end
        endmodule
        """)
    # param_i in the bad version - should have been substituted
    @test String(va.stmts[1].items[end].item.stmt.stmt.stmts[1].stmt.assign.lvalue) == "PLCFAC_i"
end

# Formal argument expansion inside macro arg list (https://github.com/CedarEDA/CedarSim.jl/issues/7)
let va = VerilogAParser.parse("""
       `define f(arg) arg
       `define g(arg) `f(arg)

       module test(x);
               analog begin
                       `g(VMAX_s) = 1;
               end
       endmodule
       """)
    # arg in the bad version - should have been substituted
    @test String(va.stmts[1].items[end].item.stmt.stmt.stmts[1].stmt.assign.lvalue) == "VMAX_s"
end

let va = VerilogAParser.parse("""
    `define f(arg) arg
    `define g(arg) `f(`f(arg))

    module test(x);
            analog begin
                    `g(VMAX_s) = 1;
            end
    endmodule
    """)
    # arg in the bad version - should have been substituted
    @test String(va.stmts[1].items[end].item.stmt.stmt.stmts[1].stmt.assign.lvalue) == "VMAX_s"
end

let va = VerilogAParser.parse("""
    `define f(a) a
    `define g(b,c) `f((b * c))

    module test(x);
    real foo;
    analog begin
        foo = `g(PBOT, one_over_one_minus_PBOT);
    end
    endmodule
    """)
    @test String(va.stmts[1].items[end].item.stmt.stmt.stmts[1].stmt.assign.rvalue.inner.rhs) == "one_over_one_minus_PBOT"
end

# Inferability of next_token
using Lexers
@test only(Base.return_types(VerilogAParser.VerilogATokenize.next_token, Tuple{Lexers.Lexer{IOBuffer, VerilogAParser.VerilogATokenize.Tokens.Kind,
                                                                          typeof(VerilogAParser.VerilogATokenize.next_token)}, Bool})) ==
    Token{VerilogAParser.VerilogATokenize.Tokens.Kind}
