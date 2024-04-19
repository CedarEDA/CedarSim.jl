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

using Lexers
@test only(Base.return_types(VerilogAParser.VerilogATokenize.next_token, Tuple{Lexers.Lexer{IOBuffer, VerilogAParser.VerilogATokenize.Tokens.Kind,
                                                                          typeof(VerilogAParser.VerilogATokenize.next_token)}, Bool})) ==
    Token{VerilogAParser.VerilogATokenize.Tokens.Kind}
