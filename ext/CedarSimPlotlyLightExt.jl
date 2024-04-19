module CedarSimPlotlyLightExt

using CedarSim
using CedarSim.SciMLBase, PlotlyLight, PlotlyLight.Cobweb

"""
    plot(sol; name_map = default_name_map, title=nothing)

Plot every top-level terminal observable in the given solution.
"""
function PlotlyLight.Plot(sol::SciMLBase.AbstractODESolution;
                          sys = CircuitIRODESystem(sol),
                          name_map::Dict = CedarSim.default_name_map(sys),
                          template::String = "plotly",
                          title::Union{Nothing,String} = nothing,
                          kwargs...)
    PlotlyLight.template!(template)
    p = Plot(;kwargs...)
    for prop in sort(collect(keys(name_map)))
        p(
            x=sol.t,
            y=sol[getproperty(sys, prop)],
            type = "scatter",
            mode="lines",
            name=name_map[prop]
        )
    end

    # Set title, if we've been given it.
    if title !== nothing
        p.layout.title = title
    end
    return p
end

"""
    save(sol, filename; name_map::Dict)

Takes a solution object from `solve!()` and writes out a `PlotlyLight`-powered HTML
webpage with self-contained, interactive plots of all observables selected by the
`name_map` dictionary.  The default mapping is given by `default_name_map()`, which
will select all top-level terminal observables if they exist.
"""
function Cobweb.save(sol::SciMLBase.AbstractODESolution, filename::String; kwargs...)
    Cobweb.save(Cobweb.Page(PlotlyLight.Plot(sol; kwargs...)), filename)
end

end # module
