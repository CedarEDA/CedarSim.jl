using VerilogAParser, AbstractTrees

va = VerilogAParser.parsefile(joinpath(@__DIR__, "../cmc_models/bsimcmg110/bsimcmg.va"));

using VerilogAParser.VerilogACSTParser: virtrange

ls = collect(Leaves(VerilogAParser.VerilogACSTParser.ChunkTree(va.ps)))

@test all(1:(length(ls)-1)) do i
    first(virtrange(ls[i+1])) == last(virtrange(ls[i]))+1
end

# 107

va = VerilogAParser.parsefile(joinpath(@__DIR__, "../cmc_models/bsimcmg107/bsimcmg.va"));

ls = collect(Leaves(VerilogAParser.VerilogACSTParser.ChunkTree(va.ps)))

@test all(1:(length(ls)-1)) do i
    first(virtrange(ls[i+1])) == last(virtrange(ls[i]))+1
end
