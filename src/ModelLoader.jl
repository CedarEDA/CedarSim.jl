module ModelLoader

using ..CedarSim
using CedarSim: VANode, VAFile

using ..VerilogAParser
using Scratch

const model_cache_dir = Ref{String}()
function __init__()
    model_cache_dir[] = @get_scratch!("model_path")
end

"""
    load_VA_model(model_path::AbstractString)

Loads and return an instance of a VA model at path `model_path`.
If a model with that name is already loaded, an instance of the loaded model is returned.
"""
function load_VA_model(model_path::AbstractString)
    file = basename(model_path)
    name = splitext(file)[1]
    module_name = name * "_vamodel"

    # Check if model of that name has already been loaded in session
    loaded_mod = get(Base.loaded_modules, Base.PkgId(nothing, module_name), nothing)
    if loaded_mod !== nothing
        return getglobal(getglobal(loaded_mod, Symbol(name * "_module")), Symbol(name))
    end

    scratch_model_path = joinpath(model_cache_dir[], module_name * ".jl")

    model_path = abspath(model_path)

    module_content = sprint() do io
        println(io, "module ", module_name)
        println(io, "using CedarSim: ModelLoader, CedarSim, VerilogAEnvironment, VAFile")
        println(io, "const srcfiles = CedarSim.parse_and_eval_vafile(@__MODULE__, VAFile(abspath($(repr(model_path)))))")
        println(io, "foreach(f -> Base.include_dependency(f.path), srcfiles)")
        println(io, "end # module")
    end

    if !isfile(scratch_model_path) || read(scratch_model_path, String) != module_content
        write(scratch_model_path, module_content)
    end

    # Setup the load path and load the model as a julia package
    old_load_path = copy(LOAD_PATH)
    old_depot_path = copy(DEPOT_PATH)
    pushfirst!(LOAD_PATH, model_cache_dir[])
    # We want to store our precompile files in a directory we control
    pushfirst!(DEPOT_PATH, model_cache_dir[])
    try
        mod = Base.require(Base.PkgId(nothing, module_name))
        return getglobal(getglobal(mod, Symbol(name * "_module")), Symbol(name))
    finally
        copy!(LOAD_PATH, old_load_path)
        copy!(DEPOT_PATH, old_depot_path)
    end
end



end # module ModelLoader
