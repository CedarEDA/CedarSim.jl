module CedarSimMakieExt

using Makie
using DAECompiler: get_sys
using CedarSim
using OrdinaryDiffEq
using StructArrays

function CedarSim.explore(sol)
    fig = Figure()

    prob = sol.prob
    # TODO this should really use some *Sim abstraction to enumerate/alter parameters
    # see https://github.com/JuliaComputing/CedarSim.jl/issues/352
    ps = sol.prob.p::ParamSim
    sys = get_sys(prob)

    ax = Axis(fig[1, 1])

    sg = SliderGrid(
        fig[1, 2],
        ((label = String(k), range = (10 .^ (-3:0.1:3)).*v, format = "{:.1e}", startvalue = v)
            for (k, v) in pairs(ps.params.params))...,
        width = 350,
        tellheight = false)

    sliderobservables = [s.value for s in sg.sliders]
    traces = lift(sliderobservables...) do slvalues...
        params = CedarSim.VectorPrism((;params=slvalues))
        # TODO this ignores spec params and breaks on nested params
        prob = remake(prob, p=ParamSim(ps.circuit, ps.mode, ps.spec, params))
        sol = solve(prob, sol.alg; initializealg=CedarDCOp())
        return sol
    end

    for (vec, name) in CedarSim.default_name_map(sys)
        trace = lift(traces) do sol
            scope = getproperty(sys, vec)
            StructArray{Point2{Float64}}((sol.t, sol[scope]))
        end
        lines!(ax, trace, label=name)
    end

    axislegend()
    fig
end

end # module
