module MTK_extension_tests
using Test
using CedarSim, ModelingToolkit, ModelingToolkitStandardLibrary.Electrical
using OrdinaryDiffEq

@mtkmodel MyResistor begin
    @parameters begin
        r = 1.0
    end
    @components begin
        Pos = Pin()
        R = Resistor(R=r)
    end
    @equations begin
        connect(R.p, Pos)
    end
end

my_resistor = MyResistor(; name=:my_resistor1)

# 1 explict pin we created at top level, 1 internal pin we reference
const MyResistorConn = @declare_MSLConnector(my_resistor, my_resistor.Pos, my_resistor.R.n)

circuit = sp"""
* A Simple Circuit with a MSL Resistor
V1 1 0 DC 10
XRes 1 0 $(MyResistorConn(r = 100.0))
"""e

circuit()  # check it doesn't error

circuit_sim = DefaultSim(circuit)
circuit_sys = CircuitIRODESystem(circuit_sim)  #, debug_config = (; store_ir_levels = true, verify_ir_levels = true, store_ss_levels = true))
circuit_prob = ODEProblem(circuit_sys, nothing, (0.0, 1.0), circuit_sim; jac=true)
circuit_sol = solve(circuit_prob, Rodas5P())

@test circuit_sol(0.0, idxs=circuit_sys.xres.R.i) == 0.1  # ohms law: i=v/r

end  # module