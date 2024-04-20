import VerilogAParser, CMC

const cmc_dir = joinpath(Base.pkgdir(CMC), "cmc_models")

models = [
    "BSIM-IMG_102.9.2_20181220/code/bsimimg.va",
    "bsimbulk106.2.0/bsimbulk.va",
    "bsimcmg110/bsimcmg.va",
    "bsimcmg107/bsimcmg.va",
    "CMC_ASM-HEMT_v101/asmhemt.va",
    "diode_cmc_160823/diode_cmc.va",
    #"hicumL2-2.4.0/hicumL2V2p4p0.va", # broken, limexp
    #"HiSIM_SOTB_1.2.0/hisimsotb.va", # broken, uses localparam?
    #"HiSIM_VA_2.5.0/hisimhv.va", # broken needs branch decl suppport
    #"hisim2_va-3.1.1/hisim2.va", # broken needs branch decl suppport
    #"hisomsoi_va-1.4.0/hisimsoi.va", # broken needs branch decl suppport
    #"mextram505/bjtd505t.va", # broken cannot handle `<c>` identifier
    #"MOSVAR130/mosvar.va", # does not terminate?
    "MVSG_CMC_APR_2018/mvsg_cmc_1.0.va",
    #"r2_cmc_v1.0.1/r2_et_cmc.va", # broken needs branch decl suppport
    #"r3_cmc_release1.1.0_2020Jun01/r3_cmc.va" # broken needs branch decl suppport
]

@testset "CMC Models" begin
    for model in models
        @testset "$model" begin
            local va = VerilogAParser.parsefile(joinpath(cmc_dir, model));
            # CMC models shouldn't have errors since they presumably work in Cadence, and others.
            buf = IOBuffer()
            out = IOContext(buf, :color=>true, :displaysize => (80, 240))
            VerilogAParser.VerilogACSTParser.visit_errors(va; io=out)

            out = String(take!(buf))
            @test isempty(out)
        end
    end
end
