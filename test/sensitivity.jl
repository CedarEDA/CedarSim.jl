module sensitivity
using Test
using CedarSim, OrdinaryDiffEq, SciMLSensitivity, DAECompiler
using CedarSim.VectorPrisms: AbstractRecordVector
using CedarSim.SpectreEnvironment
using SpectreNetlistParser

const R = CedarSim.SimpleResistor
const C = CedarSim.SimpleCapacitor
const L = CedarSim.SimpleInductor
const V(v) = CedarSim.VoltageSource(dc=v)

@testset "DefaultSim" begin
    @kwdef struct TwoResistorCircuit <: AbstractRecordVector{Float64}
        R1::Float64 = 1000.0
        R2::Float64 = 1000.0
    end

    function (self::TwoResistorCircuit)()
        vcc = Named(net, "vcc")()
        gnd = Named(net, "gnd")()
        out = Named(net, "out")()
        Named(V(1.), "V")(vcc, gnd)
        Named(R(self.R1), "R1")(vcc, out)
        Named(R(self.R2), "R2")(out, gnd)
        Named(Gnd(), "G")(gnd)
    end
    circuit = CedarSim.DefaultSim(TwoResistorCircuit(1.0, 1.0))
    sys = CircuitIRODESystem(circuit);

    sprob = ODEForwardSensitivityProblem(sys, nothing, (0.0, 1e-5), circuit)
    sol = solve(sprob, Rodas5P(autodiff=false))
    dt, (dR1, dR2) = extract_local_sensitivities(sol)

    # Sensibility tests
    ntime = size(dt, 2)
    @test ntime >= 2
    @test size(dt) == (1, ntime)
    @test size(dR1) == size(dR2) == (1, ntime)
    @test dR1 ≈ -dR2
    @test dR1 ≈ -dt/2
end

@testset "ParamLens" begin
    function two_resistor_circuit(lens)
        vcc = Named(net, "vcc")()
        gnd = Named(net, "gnd")()
        out = Named(net, "out")()
        Named(V(1.), "V")(vcc, gnd)
        Named(R(lens(R1=2000.0).R1::Float64), "R1")(vcc, out) # This R1 default gets replaced with lens.nt.R1
        Named(R(lens(R2=1.0).R2::Float64), "R2")(out, gnd) # This R2 default is passed through because !isdefined(lens.nt, :R2)
        Named(Gnd(), "G")(gnd)
    end
    circuit = ParamSim(two_resistor_circuit, var"R1" = 1.0) # This creates a `lens` object containing a NamedTuple with the actual runtime parameters
    sys = CircuitIRODESystem(circuit);

    sprob = ODEForwardSensitivityProblem(sys, nothing, (0.0, 1e-5), circuit)
    sol = solve(sprob, Rodas5P(autodiff=false))
    dt, (dR1,) = extract_local_sensitivities(sol)
    
    # Sensibility tests
    ntime = size(dt, 2)
    @test ntime >= 2
    @test size(dt) == (1, ntime)
    @test size(dR1) == (1, ntime)
    @test dR1 ≈ -dt/2
end

@testset "Parameterized PWL" begin
    spectre_code = """
    simulator lang=spectre
    global 0
    parameters i_val=20f c_val=300a r_val=1

    I1 (VDD 0) isource dc=i_val type=pwl wave=[ 0 i_val 100u i_val 110u (2*i_val) 8.11m (2*i_val) 8.12m i_val ]
    R1 (VDD VPD) resistor r=r_val
    C1 (VPD 0) capacitor c=c_val
    """
    circuit_code = CedarSim.make_spectre_circuit(
        CedarSim.SpectreNetlistParser.parse(spectre_code),
        String[],
    );
    circuit = ParamSim(eval(circuit_code), var"i_val" = 20e-12)
    sys = CircuitIRODESystem(circuit)
    sprob = ODEForwardSensitivityProblem(sys, nothing, (0.0, 8e-3), circuit)
    sol = solve(sprob, Rodas5P(autodiff=false))

    # TODO: write better tests for this
    @test length(sol.t) > 0
end

end  # module
