struct SpCircuit{CktID, Subckts}
    params::NamedTuple
    models::NamedTuple
end

function getsema end
getsema(ckt::SpCircuit{CktID}) where {CktID} = getsema(CktID)

function generate_sp_code(world::UInt64, source::LineNumberNode, ::Type{SpCircuit{CktId, Subckts}}, args...) where {CktId, Subckts}
    sig = Tuple{typeof(getsema), Type{CktId}}
    mthds = Base._methods_by_ftype(sig, -1, world)
    gen = Core.GeneratedFunctionStub(identity, Core.svec(:var"self", :args), Core.svec())
    if mthds === nothing || length(mthds) != 1
        return gen(world, source, :(getsema($CktID); error("Cedar Internal ERROR: Could not find spice method")))
    end
    match = only(mthds)

    mi = Core.Compiler.specialize_method(match)
    ci = Core.Compiler.retrieve_code_info(mi, world)
    if ci === nothing
        return gen(world, source, :(getsema($CktID); error("Cedar Internal ERROR: Could not find spice source")))
    end

    sema = ci.code[end].val
    if isa(sema, Core.SSAValue)
        sema = ci.code[sema.id]
    end
    if isa(sema, QuoteNode)
        sema = sema.value
    end
    @assert isa(sema, SemaResult)

    return gen(world, source, codegen(sema))
end

function analyze_mosfet_import(dialect, level)
    if dialect == :ngspice
        if level == 5
            #error("bsim2 not supported")
            #return :bsim2
        elseif level == 8 || level == 49
            #error("bsim3 not supported")
            #return :bsim3
        elseif level == 14 || level == 54
            return :BSIM4
        end
    end
    return nothing
end

function analyze_imports!(n::SNode, parse_cache::Union{CedarParseCache, Nothing}, traverse_imports::Bool=false;
        imports=Set{Symbol}(),
        hdl_imports=Set{String}(),
        includes::Set{String}=Set{String}(),
        pkg_hdl_imports=Set{String}(),
        pkg_spc_import=Set{String}(),
        thispath::Union{String, Nothing}=nothing)
    for stmt in n.stmts
        if isa(stmt, SNode{SP.IncludeStatement}) || isa(stmt, SNode{SP.LibInclude}) || isa(stmt, SNode{SP.HDLStatement})
            str = strip(unescape_string(String(stmt.path)), ['"', '\'']) # verify??
            if startswith(str, JLPATH_PREFIX)
                path = str[sizeof(JLPATH_PREFIX)+1:end]
                components = splitpath(path)
                push!(imports, Symbol(components[1]))
                if isa(stmt, SNode{SP.HDLStatement})
                    push!(pkg_hdl_imports, str)
                else
                    push!(pkg_spc_import, str)
                end
            else
                if thispath !== nothing
                    str = joinpath(dirname(thispath), str)
                end
                if parse_cache !== nothing
                    if isa(stmt, SNode{SP.HDLStatement})
                        parse_and_cache_va!(parse_cache, str)
                    else
                        str in includes && continue
                        push!(includes, str)
                        analyze_imports!(parse_and_cache_spc!(parse_cache, str), parse_cache; imports, hdl_imports, includes, thispath=str)
                    end
                else
                    if isa(stmt, SNode{SP.HDLStatement})
                        push!(hdl_imports, str)
                    else
                        push!(includes, str)
                    end
                end
            end
        elseif isa(stmt, SNode{SP.Model})
            typ = LSymbol(stmt.typ)
            mosfet_type = typ in (:nmos, :pmos) ? typ : nothing
            local level = 1
            for p in stmt.parameters
                name = LSymbol(p.name)
                if name == :level
                    # TODO
                    level = parse(Float64, String(p.val))
                    continue
                end
            end
            mosfet_type === nothing && continue
            imp = analyze_mosfet_import(:ngspice, level)
            imp !== nothing && push!(imports, imp)
        elseif isa(stmt, Union{SNode{SPICENetlistSource}, SNode{SP.Subckt}, SNode{SP.LibStatement}})
            analyze_imports!(stmt, parse_cache; imports, hdl_imports, includes, pkg_hdl_imports, pkg_spc_import, thispath)
        end
    end
    return imports, hdl_imports, includes, pkg_hdl_imports, pkg_spc_import
end

function ensure_cache!(mod::Module)
    if isdefined(mod, :var"#cedar_parse_cache#")
        return mod.var"#cedar_parse_cache#"
    end
    cache = CedarParseCache(mod)
    Core.eval(mod, :(const var"#cedar_parse_cache#" = $cache))
    return cache
end

function codegen_missing_imports!(thismod::Module, imps::Union{Dict{Symbol, Module}, NamedTuple}, pkg_hdl_imports::Set{String}, pkg_spc_import::Set{String})
    if isa(imps, NamedTuple)
        imps = Dict{Symbol, Module}(pairs(imps)...)
    end
    for imp in pkg_spc_import
        @assert startswith(imp, JLPATH_PREFIX)
        path = imp[sizeof(JLPATH_PREFIX)+1:end]
        components = splitpath(path)
        mod = imps[Symbol(components[1])]
        localpath = joinpath(components[2:end])
        cache = ensure_cache!(mod)
        if haskey(cache.spc_cache, localpath)
            continue
        end
        imports, _, _, sub_pkg_hdl_imports, sub_pkg_spc_import = analyze_imports!(parse_and_cache_spc!(cache, localpath), cache, thispath=localpath)
        sub_imps = Dict{Symbol, Module}(Symbol(pkg) => Base.require(mod, Symbol(pkg)) for pkg in imports)
        codegen_missing_imports!(mod, sub_imps, sub_pkg_hdl_imports, sub_pkg_spc_import)
    end
    for imp in pkg_hdl_imports
        @assert startswith(imp, JLPATH_PREFIX)
        path = imp[sizeof(JLPATH_PREFIX)+1:end]
        components = splitpath(path)
        mod = imps[Symbol(components[1])]
        localpath = joinpath(components[2:end])
        cache = ensure_cache!(mod)
        codegen_hdl_import!(mod, cache, localpath)
    end
end

function codegen_hdl_import!(mod::Module, cache::CedarParseCache, imp::String)
    va = get(cache.va_cache, imp, nothing)
    va isa Pair && return
    if va === nothing
        va = parse_and_cache_va!(cache, imp)
    end

    vamod = va.stmts[end]
    s = gensym(String(vamod.id))
    sm = Core.eval(mod, :(baremodule $s
        const VerilogAEnvironment = $(CedarSim.VerilogAEnvironment)
        using .VerilogAEnvironment
        $(CedarSim.make_spice_device(vamod))
        const $(Symbol(lowercase(String(vamod.id)))) = $(Symbol(vamod.id))
    end))

    recache_va!(mod, imp, Pair{VANode, Module}(va, sm))
end

function codegen_hdl_imports!(mod::Module, hdl_imports)
    cache = mod.var"#cedar_parse_cache#"
    for imp in hdl_imports
        codegen_hdl_import!(mod, cache, imp)
    end
end

macro sp_str(str, flag="")
    include_paths = [dirname(String(__source__.file)), pwd()]
    enable_julia_escape = 'e' in flag
    inline = 'i' in flag
    sa = SpectreNetlistParser.parse(IOBuffer(str); start_lang=:spice, enable_julia_escape,
        implicit_title = !inline, fname=String(__source__.file), line_offset=__source__.line-1)
    imports, hdl_imports, includes, pkg_hdl_imports, pkg_spc_import = analyze_imports!(sa, nothing)
    if isempty(imports) && isempty(hdl_imports) && isempty(includes)
        # No imports or includes at all, can avoid all the caching complexity
        return sema_assign_ids(sema(sa))
    end
    # See if we have a cache already. TODO: Concurrent import.
    new_cache = false
    if isdefined(__module__, :var"#cedar_parse_cache#")
        cache = __module__.var"#cedar_parse_cache#"
    else
        cache = CedarParseCache(__module__)
        new_cache = true
    end

    for include in includes
        analyze_imports!(parse_and_cache_spc!(parse_cache, str), cache; imports, hdl_imports, includes, thispath=include)
    end

    t = Expr(:toplevel)
    if new_cache
        push!(t.args, esc(:(const var"#cedar_parse_cache#" = $cache)))
    end
    hdls = Expr[]
    kws = Expr[]
    # Codegen top-level HDL imports
    push!(t.args, Expr(:call, codegen_hdl_imports!, __module__, hdl_imports))
    for imp in imports
        s = gensym()
        imp = Symbol(imp)
        push!(t.args, :(import $imp as $s))
        push!(kws, Expr(:kw, imp, s))
    end
    push!(t.args, esc(quote
        let imps=(;$(kws...))
            $(codegen_missing_imports!)($(__module__), imps, $(pkg_hdl_imports), $(pkg_spc_import))
            eval($(sema_assign_ids)($(sema)($sa; imps, parse_cache=var"#cedar_parse_cache#")))
        end
    end))
    return t
end
