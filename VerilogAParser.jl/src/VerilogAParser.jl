module VerilogAParser
include("tokenize/VerilogATokenize.jl")
include("parse/VerilogACSTParser.jl")
using .VerilogACSTParser: parse, parsefile, EXPR, Node

using SnoopPrecompile
@precompile_all_calls begin
    parsefile(joinpath(@__DIR__, "../cmc_models/bsimcmg107/bsimcmg.va"))
end

end
