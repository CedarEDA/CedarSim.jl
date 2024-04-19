module CedarSimCSVExt

using CSV, CedarSim
using CedarSim.SciMLBase: AbstractODESolution

# Write out a set of probe points to the given `file`
"""
    CSV.write(file, sol)

Write all default ScopeRef's out to a `.csv` file.  Use `name_map` keyword argument to
control selection and naming of which ScopeRef's should be written out to .CSV.
"""
function CSV.write(file::AbstractString, sol::AbstractODESolution; sys = CircuitIRODESystem(sol), name_map::Dict{Symbol,String} = CedarSim.default_name_map(sys))
    data = (;
        t = sol.t,
        ((Symbol(name), sol[getproperty(sys, prop)]) for (prop, name) in name_map)...,
    )
    return CSV.write(file, data)
end

end # module
