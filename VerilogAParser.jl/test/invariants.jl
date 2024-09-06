import VerilogAParser, CMC
using AbstractTrees
using VerilogAParser.VerilogACSTParser: virtrange


macro_va = VerilogAParser.parse("""
    `define f(arg) arg
    `define g(arg) `f(arg)

    module test(x);
            analog begin
                    `g(VMAX_s) = 1;
            end
    endmodule
    """)

for va in (
        VerilogAParser.parsefile(joinpath(Base.pkgdir(CMC), "cmc_models/bsimcmg110/bsimcmg.va")),
        VerilogAParser.parsefile(joinpath(Base.pkgdir(CMC), "cmc_models/bsimcmg107/bsimcmg.va")),
        macro_va)

    ls = collect(Leaves(VerilogAParser.VerilogACSTParser.ChunkTree(va.ps)))
    @test all(1:(length(ls)-1)) do i
        first(virtrange(ls[i+1])) == last(virtrange(ls[i]))+1
    end
end
