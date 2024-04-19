module params_tests

include("common.jl")

struct ParCir
    R::Float64
    V::Float64
end
ParCir() = ParCir(2.0, 5.0)
function (ckt::ParCir)()
    vcc = Named(net, "vcc")()
    gnd = Named(net, "gnd")()
    Named(V(ckt.V), "V")(vcc, gnd)
    Named(R(ckt.R), "R")(vcc, gnd)
    Gnd()(gnd)
end

sim = ParamSim(ParCir, R=1.0, temp=340.0)
sys, sol = solve_circuit(sim)
@test sol[sys.V.I][end] == -5.0



Base.@kwdef struct NestedParCir
    child::ParCir = ParCir()
end
function (ckt::NestedParCir)()
    Named(SubCircuit(ckt.child), "child")()
end

sim = ParamSim(NestedParCir, var"child.R"=1.0, temp=340.0)
sys, sol = solve_circuit(sim)
@test sol[sys.child.V.I][end] == -5.0



function FuncCir(lens)
    vcc = Named(net, "vcc")()
    gnd = Named(net, "gnd")()
    Named(V(lens(V=5.0).V::Float64), "V")(vcc, gnd)
    Named(R(lens(R=2.0).R::Float64), "R")(vcc, gnd)
    Gnd()(gnd)
end

sim = ParamSim(FuncCir, var"R"=1.0, temp=340.0)
sys, sol = solve_circuit(sim)
@test sol[sys.V.I][end] == -5.0

# test parameter exploration
import CairoMakie
fig = CedarSim.explore(sol)
plots_dir = joinpath(Base.pkgdir(CedarSim), "test", "plots")
mkpath(plots_dir)
CairoMakie.save(joinpath(plots_dir, "explore.png"), fig)

spice_ckt = """
* Subcircuit parameters
.subckt inner a b foo=foo+2000
R1 a b r= 'foo'
.ends

.subckt outer a b
x1 a b inner
.ends

.param inner  =1
.param foo =  1
i1 vcc 0 'foo'
l1 vcc out 1m
x1 out 0 outer
"""

ast = SpectreNetlistParser.SPICENetlistParser.parse(spice_ckt)
code = CedarSim.make_spectre_circuit(ast)
circuit = eval(code)

observer = CedarSim.ParamObserver(foo=200);
circuit(observer);
observed = convert(NamedTuple, observer)
expected = (
    x1 = (; x1 = (; r1 = (r = 2200,), foo = 2200)),
    i1 = (; dc = 200,), foo = 200, l1 = (; l = 0.001,), inner = 1
)

# `observed` may contain extra fields, but it must agree on all of `expected`s fields
⊑(x::NamedTuple, y::NamedTuple) = all(haskey(x, k) && (x[k] == y[k] || x[k] ⊑ y[k]) for k in keys(y))
@test observed ⊑ expected

# test that we can pass parameters to subcircuits
sim = CedarSim.ParamSim(circuit; x1=(x1=(foo=2.0,),))
sys, sol = solve_circuit(sim);
@test isapprox_deftol(sol[sys.x1.x1.r1.V][end], -2)

sim = CedarSim.ParamSim(circuit; foo=2.0)
sys, sol = solve_circuit(sim);
@test isapprox_deftol(sol[sys.x1.x1.r1.V][end], -4004.0)

sim = CedarSim.ParamSim(circuit; x1=(x1=(r1=(r=100.0,),),))
sys, sol = solve_circuit(sim);
@test isapprox_deftol(sol[sys.x1.x1.r1.V][end], -100)

# Test that our 'default parameterization' helper sees `foo` and `inner`
default_params = CedarSim.get_default_parameterization(ast)
@test (:inner => 1.0) ∈ default_params
@test (:foo => 1.0) ∈ default_params

io = IOBuffer()
CedarSim.alter(io, ast, foo=2.0, inner=(foo=3.0, r1=(r=4.0,)))
modified = String(take!(io))
replaced = replace(spice_ckt,
    "foo =  1" => "foo =  2.0",
    "foo=foo+2000" => "foo=3.0",
    "r= 'foo'" => "r= 4.0")
@test modified == replaced
new_ast = SpectreNetlistParser.SPICENetlistParser.parse(modified)
default_params = CedarSim.get_default_parameterization(new_ast)
@test (:foo => 2) ∈ default_params

CedarSim.alter(io, ast, ParamSim(circuit; foo=2.0, inner=(foo=3.0, r1=(r=4.0,))))
modified = String(take!(io))
@test modified == replaced

CedarSim.alter(io, ast, CedarSim.ParamLens((foo=2.0, inner=(foo=3.0, r1=(r=4.0,)))))
modified = String(take!(io))
@test modified == replaced

end # module params_tests
