module compiler_sanity

using CedarSim, DAECompiler, BSIM4, Test

const bsim4 = load_VA_model(BSIM4.bsim4_va)

let f = CedarSim.find_bin, args = Tuple{
    CedarSim.BinnedModel{Tuple{CedarSim.ParsedModel{bsim4}, CedarSim.ParsedModel{bsim4}}},
    Float64, Float64}

    # find_bin must be concrete-eval eligible or we don't get any constant propagation
    @test Core.Compiler.is_foldable(Base.infer_effects(f, args))

    # Check the same with the DAEInterpreter, which should be the same, but let's
    # verify
    let (interp, frame) = DAECompiler.typeinf_dae(Tuple{typeof(f), args.parameters...})
        @test Core.Compiler.is_foldable(frame.ipo_effects)
    end
end

end
