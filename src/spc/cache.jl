struct CedarParseCache
    thismod::Module
    spc_cache::Dict{String, Any #=SemaResult=#}
    va_cache::Dict{String, Union{VANode, Pair{VANode, Module}}}
end
CedarParseCache(m::Module) = CedarParseCache(m, Dict{String, Any #=SemaResult=#}(), Dict{String, Pair{VANode, Module}}())

function parse_and_cache_spc!(cache::CedarParseCache, path::String)
    if haskey(cache.spc_cache, path)
        return cache.spc_cache[path]
    end
    abspath = isabspath(path) ? path : joinpath(dirname(pathof(cache.thismod)), "..", path)
    sa = SpectreNetlistParser.parsefile(abspath; implicit_title=false)
    cache_spc!(cache.thismod, path, sa; abspath)
    return sa
end
parse_and_cache_spc!(cache::CedarParseCache, path::AbstractString) = parse_and_cache_spc!(cache, String(path))

function parse_and_cache_va!(cache::CedarParseCache, path::String)
    if haskey(cache.va_cache, path)
        return cache.va_cache[path]
    end
    abspath = isabspath(path) ? path : joinpath(dirname(pathof(cache.thismod)), "..", path)
    va = VerilogAParser.parsefile(abspath)
    cache_va!(cache.thismod, path, va; abspath)
    return va
end
parse_and_cache_va!(cache::CedarParseCache, path::AbstractString) = parse_and_cache_va!(cache, String(path))


function cache_spc!(thismod::Module, path::String, sr #=::SemaResult=#; abspath=nothing)
    ccall(:jl_check_top_level_effect, Cvoid, (Any, Cstring), thismod, "cache_spc!")
    thismod.var"#cedar_parse_cache#".spc_cache[path] = sr
    include_dependency(abspath !== nothing ? abspath : path)
end

function recache_spc!(thismod::Module, path::String, sr #=::SemaResult=#)
    ccall(:jl_check_top_level_effect, Cvoid, (Any, Cstring), thismod, "recache_spc!")
    existing = get(thismod.var"#cedar_parse_cache#".spc_cache, path, nothing)
    @assert existing !== nothing && isa(existing, SNode)
    thismod.var"#cedar_parse_cache#".spc_cache[path] = sr
end

function cache_va!(thismod::Module, path::String, vm::Union{VANode, Pair{VANode, Module}}; abspath=nothing)
    ccall(:jl_check_top_level_effect, Cvoid, (Any, Cstring), thismod, "cache_va!")
    @assert !haskey(thismod.var"#cedar_parse_cache#".va_cache, path)
    thismod.var"#cedar_parse_cache#".va_cache[path] = vm
    # TODO: Verilog-side includes
    include_dependency(abspath !== nothing ? abspath : path)
end

function recache_va!(thismod::Module, path::String, vm::Pair{VANode, Module})
    ccall(:jl_check_top_level_effect, Cvoid, (Any, Cstring), thismod, "recache_va!")
    existing = get(thismod.var"#cedar_parse_cache#".va_cache, path, nothing)
    @assert existing !== nothing && isa(existing, VANode)
    thismod.var"#cedar_parse_cache#".va_cache[path] = vm
end
