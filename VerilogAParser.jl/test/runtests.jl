using VerilogAParser
using VerilogAParser: EXPR, Node
using VerilogAParser.VerilogACSTParser: VerilogSource
using Test

var = """
    module BasicVAResistor(p, n); // N.B.: Whitespace at the beginning of this line is part of the test

inout p, n;
electrical p, n;
parameter real R=1 exclude 0;

analog begin
    I(p,n) <+ V(p,n)/R;
end
endmodule
"""

# For now just test that this doesn't error
let res = VerilogAParser.parse(var)
    @test isa(res, Node{VerilogSource})
    @test UInt32(res.startof) == 0
end

include("sv_tests.jl")
include("invariants.jl")
include("regression.jl")
include("errors.jl")
include("cmc_models.jl")