# We have a `Project.toml` here that provides some docs-specific packages
# such as `Weave`, `Catlab`, `GraphViz`, etc...
# This allows us to push the main `CedarSim` project onto the fron of the stack,
# so that we can load `CedarSim` without having to maintain a separate project here.
insert!(Base.LOAD_PATH, 1, dirname(@__DIR__))

using Weave
# preload so that we don't get conda output in the documentation
include("circuit.jl")

# By default, we want to stop execution if one of our code chunks fails.
# To explicitly allow this, use the `; error = false` syntax in your code chunk.
# See e.g. `circuit_debugging.jmd` which does this a lot.
Weave.set_chunk_defaults!(:error => false)

# Generate a dynamic `index.jmd`
open(joinpath(@__DIR__, "index.jmd"), write=true) do io
    println(io, "# CedarSim developer documentation index")
    println(io)
    for jmd_file in filter(f -> endswith(f, ".jmd"), readdir(@__DIR__))
        filename = splitext(jmd_file)[1]
        println(io, "* [$(filename)]($(filename).html)")
        println(io)
    end
end

for jmd_file in filter(f -> endswith(f, ".jmd"), readdir(@__DIR__; join=true))
    weave(jmd_file; doctype = "md2html", mod=Main, cache=:off)
end
