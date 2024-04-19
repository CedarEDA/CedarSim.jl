module bins_test

using CedarSim
using CedarSim.SpectreEnvironment
using SpectreNetlistParser
using Test
using SpectreNetlistParser: SPICENetlistParser
using BSIM4

const bsim4 = load_VA_model(BSIM4.bsim4_va)

inc = @__DIR__
nl = inc * "/bins.cir"
sa1 = SPICENetlistParser.parsefile(nl)
code = CedarSim.make_spectre_netlist(sa1, [inc])
circuit = eval(code)

@testset "model binning" begin
    @test CedarSim.spicecall(nmos_3p3, l=2.8e-007, w=2.2e-007).device == var"nmos_3p3.0"(L=2.8e-007, W=2.2e-007)
    @test CedarSim.spicecall(nmos_3p3, l=5.0e-007, w=2.2e-007).device == var"nmos_3p3.1"(L=5.0e-007, W=2.2e-007)
end

end # module bins_test
