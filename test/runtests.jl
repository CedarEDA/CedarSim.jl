using Test
using Random

# avoid random CI failures due to randomness
# fixed seed chosen by fair dice roll
Random.seed!(10)

@testset "basic.jl" include("basic.jl")
@testset "transients.jl" include("transients.jl")
@testset "compilation.jl" include("compilation.jl")
@testset "params.jl" include("params.jl")
@testset "ddx.jl" include("ddx.jl")
@testset "alias.jl" include("alias.jl")
@testset "varegress.jl" include("varegress.jl")
@testset "compiler_sanity.jl" include("compiler_sanity.jl")
@testset "binning/bins.jl" include("binning/bins.jl")
@testset "bsimcmg/demo_bsimcmg.jl" include("bsimcmg/demo_bsimcmg.jl")
#@testset "bsimcmg/bsimcmg_spectre.jl" include("bsimcmg/bsimcmg_spectre.jl")
@testset "bsimcmg/inverter.jl" include("bsimcmg/inverter.jl")
@testset "ac.jl" include("ac.jl")
@testset "sky130" include("sky130/parse_unified.jl")
@testset "spectre_expr.jl" include("spectre_expr.jl")
@testset "sweep.jl" include("sweep.jl")
@testset "inverter.jl" include("inverter.jl")
@testset "gf180_dff.jl" include("gf180_dff.jl")
@testset "sensitivity.jl" include("sensitivity.jl")
@testset "inverter_noise.jl" include("inverter_noise.jl")
@testset "MTK_extension.jl" include("MTK_extension.jl")