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

function analyze_imports!(n::SNode; imports=Set{String}())
    for stmt in n.stmts
        if isa(stmt, SNode{SP.IncludeStatement}) || isa(stmt, SNode{SP.LibInclude})
            str = strip(unescape_string(String(stmt.path)), ['"', '\'']) # verify??
            if startswith(str, JLPATH_PREFIX)
                path = str[sizeof(JLPATH_PREFIX)+1:end]
                components = splitpath(path)
                push!(imports, components[1])
            end
        elseif isa(stmt, SNode{SP.Model})
            typ = LSymbol(model.typ)
            mosfet_type = typ in (:nmos, :pmos) ? typ : nothing
            local level = 1
            for p in stmt.parameters
                name = LSymbol(p.name)
                if name == :type
                    name = :devtype
                    val = LSymbol(p.val)
                    @assert val in (:p, :n)
                    mosfet_type = val == :p ? :pmos : :nmos
                    continue
                elseif name == :level
                    # TODO
                    level = Int(parse(Float64, String(p.val)))
                    continue
                end
            end
            mosfet_type === nothing && continue
            imp = analyze_mosfet_import(:ngspice, level)
            imp !== nothing && push!(imports, imp)
        elseif isa(stmt, Union{SNode{SPICENetlistSource}, SNode{SP.Subckt}, SNode{SP.LibStatement}})
            analyze_imports!(stmt, imports=imports)
        end
    end
    return imports
end

macro sp_str(str, flag="")
    include_paths = [dirname(String(__source__.file)), pwd()]
    enable_julia_escape = 'e' in flag
    inline = 'i' in flag
    sa = SpectreNetlistParser.parse(IOBuffer(str); start_lang=:spice, enable_julia_escape,
        implicit_title = !inline, fname=String(__source__.file), line_offset=__source__.line-1)
    imports = analyze_imports!(sa)
    if isempty(imports)
        return sema_assign_ids(sema(sa))
    end
    t = Expr(:toplevel)
    kws = Expr[]
    for imp in imports
        s = gensym()
        imp = Symbol(imp)
        push!(t.args, :(import $imp as $s))
        push!(kws, Expr(:kw, imp, s))
    end
    push!(t.args, :(eval($(sema_assign_ids)($(sema)($sa; imps=(;$(kws...)))))))
    return t
end
