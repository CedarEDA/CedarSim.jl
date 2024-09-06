using OrderedCollections
using Graphs
using VerilogAParser

const JLPATH_PREFIX = "jlpkg://"

struct MaybeConditional{T}
    cond::Int # 0 indicates not conditional, negative indicates inverted
    val::T
end
Base.convert(::Type{MaybeConditional{T}}, x::T) where T = MaybeConditional{T}(0, x)
Base.convert(::Type{MaybeConditional{SNode}}, x::SNode) = MaybeConditional{SNode}(0, x)
Base.convert(::Type{MaybeConditional{Pair{SNode, GlobalRef}}}, x::Pair{<:SNode, GlobalRef}) = MaybeConditional{Pair{SNode, GlobalRef}}(0, x)
Base.convert(::Type{MaybeConditional{T}}, x::MaybeConditional) where {T} = MaybeConditional{T}(x.cond, convert(T, x.val))

@enum CircuitKind begin
    SPICECircuit
    SpectreCircuit
    Mixed
end

struct SemaSpec
    ast::SNode
    imps::Union{Nothing, Module, Dict{Symbol, Module}}
    parse_cache::Union{Nothing, CedarParseCache}
    imported_hdl_modules::Vector{Module}
end

mutable struct SemaResult
    ast::SNode
    CktID::Union{Nothing, Type}
    parse_cache::Union{Nothing, CedarParseCache}

    title::Union{SNode, Nothing}
    global_position::UInt64
    kind::CircuitKind

    conditionals::Vector{Pair{UInt, MaybeConditional{SNode}}}
    condition_stack::Vector{Int}

    nets::Dict{Symbol, Vector{Pair{UInt, MaybeConditional{SNode}}}}
    params::OrderedDict{Symbol, Vector{Pair{UInt, MaybeConditional{SNode}}}}
    models::Dict{Symbol, Vector{Pair{UInt, MaybeConditional{Pair{SNode, GlobalRef}}}}}
    subckts::Dict{Symbol, Vector{Pair{UInt, MaybeConditional{SemaResult}}}}
    libs::Dict{Symbol, Vector{Union{Pair{UInt, SemaSpec}, Pair{UInt, SemaResult}}}}
    options::Dict{Symbol, Vector{Pair{UInt, SNode}}}

    instances::OrderedDict{Symbol, Vector{Pair{UInt, MaybeConditional{SNode}}}}
    hdl_includes::Vector{Pair{UInt, Pair{SNode, VANode}}}

    warnings::Vector

    # While processing sema, all seen parameters/subckts/models. Upon completion,
    # only those that do not have local defs
    exposed_parameters::OrderedSet{Symbol}
    exposed_subckts::OrderedSet{Symbol}
    exposed_models::OrderedSet{Symbol}

    formal_parameters::OrderedSet{Symbol}
    parameter_order::Vector{Int}

    imported_hdl_modules::Vector{Module}

    imps::Union{Nothing, Module, Dict{Symbol, Module}}
end

function SemaResult(ast::SNode)
    SemaResult(ast, nothing, nothing, nothing, UInt64(0), SpectreCircuit,
        Vector{Pair{UInt, MaybeConditional{SNode}}}(), Vector{UInt}(),
        Dict(), Dict(), Dict(), Dict(), Dict(), Dict(), Dict(), Vector(), Vector(),
        OrderedSet{Symbol}(), OrderedSet{Symbol}(), OrderedSet{Symbol}(),
        OrderedSet{Symbol}(),
        Int[], Module[], nothing)
end

function MaybeConditional(scope::SemaResult, stmt)
    MaybeConditional(isempty(scope.condition_stack) ? 0 : scope.condition_stack[end], stmt)
end

function offset_cd(cd::MaybeConditional{T}, offset) where {T}
    MaybeConditional{T}(cd.cond == 0 ? 0 : cd.cond + offset, cd.val)
end
offset_cd(cd::Pair{UInt, <:MaybeConditional}, offset) = cd[1]=>offset_cd(cd[2], offset)

function SemaSpec(scope::SemaResult, ast::SNode)
    SemaSpec(ast, scope.imps, scope.parse_cache, scope.imported_hdl_modules)
end

const TwoTerminal = Union{SNode{SP.Resistor}, SNode{SP.Inductor}, SNode{SP.Capacitor},
    SNode{SP.Diode}, SNode{SP.Voltage}, SNode{SP.Current}}

const AnySPInstance = Union{SNode{SP.MOSFET}, SNode{SP.SubcktCall}, SNode{SP.VAModelCall},
    SNode{SP.BipolarTransistor}, SNode{SP.Behavioral},
    SNode{SP.CCVS}, SNode{SP.CCCS}, SNode{SP.VCVS}, SNode{SP.VCCS},
    SNode{SP.Switch}, SNode{SP.JuliaDevice}, TwoTerminal}

function warn!(scope::SemaResult, warn)

end

sema_nets(instance::TwoTerminal) = (instance.pos, instance.neg)
sema_nets(instance::SNode{SP.MOSFET}) = (instance.d, instance.g, instance.s, instance.b)
sema_nets(instance::SNode{SP.BipolarTransistor}) = (instance.c, instance.b, instance.e, instance.s)
sema_nets(instance::Union{SNode{SP.SubcktCall}, SNode{SP.VAModelCall}}) = instance.nodes

"""
    SPICE/Spectre codegen pass 1
"""
function sema!(scope::SemaResult, n::SNode{<:SC.AbstractBlockASTNode})
    for stmt in n.stmts
        if isa(stmt, SNode{SPICENetlistSource})
            sema!(scope, stmt)
        else
            @show stmt
            error()
        end
    end
end

function sema_visit_ids!(f, expr::Union{SNode{SC.Identifier}, SNode{SP.Identifier}})
    id = LSymbol(expr)
    if id in (:var"true", :var"false", :var"\$time", :var"temper")
        return
    end
    f(id)
end

function sema_visit_ids!(f, expr::Union{SNode{SC.Parens}, SNode{SP.Parens}, SNode{SP.Prime}, SNode{SP.Brace}})
    return sema_visit_ids!(f, expr.inner)
end

function sema_visit_ids!(f, expr::Union{SNode{SC.TernaryExpr}, SNode{SP.TernaryExpr}})
    sema_visit_ids!(f, expr.condition)
    sema_visit_ids!(f, expr.ifcase)
    sema_visit_ids!(f, expr.elsecase)
end

function sema_visit_ids!(f, expr::SNode{SP.BinaryExpression})
    sema_visit_ids!(f, expr.lhs)
    sema_visit_ids!(f, expr.rhs)
end

function sema_visit_ids!(f, expr::SNode{SP.DCSource})
    sema_visit_ids!(f, expr.dcval)
end

function sema_visit_ids!(f, expr::SNode{SP.TranSource})
    for val in expr.values
        sema_visit_ids!(f, val)
    end
end

function sema_visit_ids!(f, expr::Union{SNode{SC.FunctionCall}, SNode{SP.FunctionCall}})
    for arg in expr.args
        sema_visit_ids!(f, arg.item)
    end
end

function sema_visit_ids!(f, cs::Union{SNode{SC.UnaryOp}, SNode{SP.UnaryOp}})
    sema_visit_ids!(f, cs.operand)
end

function sema_visit_ids!(f, cs::SNode{SP.Parameter})
    # TODO: Lot/Dev?
    sema_visit_ids!(f, cs.val)
end

function sema_visit_ids!(f, cs::SNode{SP.Condition})
    sema_visit_ids!(f, cs.body)
end


sema_visit_ids!(f, expr::Union{SNode{SP.NumericValue}, SNode{SC.NumericValue}}) = nothing
sema_visit_ids!(f, ::Nothing) = nothing

function sema_visit_expr!(scope::SemaResult, @nospecialize(expr))
    sema_visit_ids!(expr) do id
        push!(scope.exposed_parameters, id)
    end
end

function get_section!(scope::SemaResult, name::Symbol)
    if haskey(scope.libs, name)
        sect = scope.libs[name][end]
        if !isa(sect[2], SemaResult)
            spec = sect[2]::SemaSpec
            semad = sema_file_or_section(spec.ast; imps=spec.imps, parse_cache=spec.parse_cache, imported_hdl_modules=scope.imported_hdl_modules)
            scope.libs[name][end] = sect[1]=>semad
            return semad
        end
        return sect[2]
    end
end

function sema_resolve_import(scope::SemaResult, imp::Symbol)
    imp_mod = nothing
    if scope.imps !== nothing
        if isa(scope.imps, Module)
            imp_mod = Base.require(scope.imps, imp)
        else
            imp_mod = get(scope.imps, imp, nothing)
        end
    end
    imp_mod === nothing && error("Import $imp not provided to sema")
    return imp_mod
end

function spice_select_device(sema::SemaResult, devkind::Symbol, level, version, stmt; dialect=:ngspice)
    if devkind == :d
        return GlobalRef(CedarSim.SpectreEnvironment, :diode)
    elseif devkind == :r
        return GlobalRef(CedarSim.SpectreEnvironment, :resistor)
    elseif devkind == :c
        return GlobalRef(CedarSim.SpectreEnvironment, :capacitor)
    end
    if dialect == :ngspice
        if devkind in (:pmos, :nmos)
            if level == 5
                file = stmt.ps.srcfile.path
                line = SpectreNetlistParser.LineNumbers.compute_line(stmt.ps.srcfile.lineinfo, stmt.startof)
                @warn "BSIM2 ($devkind at level $level) not implemented" _file=file _line=line
                return GlobalRef(CedarSim, :UnimplementedDevice)
            elseif level == 8 || level == 49
                file = stmt.ps.srcfile.path
                line = SpectreNetlistParser.LineNumbers.compute_line(stmt.ps.srcfile.lineinfo, stmt.startof)
                @warn "BSIM3 ($devkind at level $level) not implemented" _file=file _line=line
                return GlobalRef(CedarSim, :UnimplementedDevice)
            elseif level == 14 || level == 54
                return GlobalRef(sema_resolve_import(sema, :BSIM4), :bsim4)
            elseif level == 17 || level == 72
                if version == 107 || version === nothing
                    return :bsimcmg107
                else
                    file = stmt.ps.srcfile.path
                    line = SpectreNetlistParser.LineNumbers.compute_line(stmt.ps.srcfile.lineinfo, stmt.startof)
                    @warn "Version $version of mosfet $devkind at level $level not implemented" _file=file _line=line
                    return GlobalRef(CedarSim, :UnimplementedDevice)
                end
            else
                file = stmt.ps.srcfile.path
                line = SpectreNetlistParser.LineNumbers.compute_line(stmt.ps.srcfile.lineinfo, stmt.startof)
                @warn "Mosfet $devkind at level $level not implemented" _file=file _line=line
                return GlobalRef(CedarSim, :UnimplementedDevice)
            end
        elseif devkind in (:pnp, :npn)
            file = stmt.ps.srcfile.path
            line = SpectreNetlistParser.LineNumbers.compute_line(stmt.ps.srcfile.lineinfo, stmt.startof)
            @warn "Bipolar $devkind at level $level not implemented" _file=file _line=line
            return GlobalRef(CedarSim, :UnimplementedDevice)
        elseif devkind == :sw
            return GlobalRef(CedarSim.SpectreEnvironment, :Switch)
        end
    end
    # Search for this model in the HDL Imports
    if sema.parse_cache !== nothing
        # Might be an import instead, which we will resolve later
        return GlobalRef(sema.parse_cache.thismod, devkind)
    end
    file = stmt.ps.srcfile.path
    line = SpectreNetlistParser.LineNumbers.compute_line(stmt.ps.srcfile.lineinfo, stmt.startof)
    @warn "Device $devkind at level $level not implemented" _file=file _line=line
    return GlobalRef(CedarSim, :UnimplementedDevice)
end

function sema_visit_model!(scope::SemaResult, stmt::SNode{SP.Model})
    name = LSymbol(stmt.name)
    if haskey(scope.models, name)
        warn!(scope, "Duplicate model definition $name")
    end

    typ = LSymbol(stmt.typ)
    level = nothing
    version = nothing
    for param in stmt.parameters
        sema_visit_expr!(scope, param.val)

        pname = LSymbol(param.name)
        if pname == :level
            # TODO
            level = parse(Float64, String(param.val))
            continue
        elseif pname == :version
            version = parse(Float64, String(param.val))
            continue
        end
    end
    modelref = spice_select_device(scope, typ, level, version, stmt)

    push!(get!(()->[], scope.models, name), scope.global_position=>(stmt=>modelref))
end

function sema!(scope::SemaResult, n::Union{SNode{SPICENetlistSource}, SNode{SP.Subckt}, SNode{SP.LibStatement}, SNode{SP.IfElseCase}})
    if isa(n, SNode{SP.Subckt})
        for param in n.parameters
            name = LSymbol(param.name)
            push!(scope.formal_parameters, name)
            push!(get!(()->[], scope.params, name), scope.global_position=>param)
            sema_visit_expr!(scope, param.val)
        end
    end
    for stmt in n.stmts
        if isa(stmt, SNode{SP.Title})
            if scope.title !== nothing
                warn!(scope, "Duplicate title definition")
            end
            scope.title = stmt
        elseif isa(stmt, SNode{SC.Instance}) || isa(stmt, AnySPInstance)
            name = LSymbol(stmt.name)
            if haskey(scope.instances, name) && isempty(scope.condition_stack)
                error("Duplicate instance name $name")
            end
            push!(get!(()->[], scope.instances, name), scope.global_position=>MaybeConditional(scope, stmt))
            for net in sema_nets(stmt)
                push!(get!(()->[], scope.nets, LSymbol(net)), scope.global_position=>net)
            end
            if isa(stmt, Union{SNode{SP.Resistor}, SNode{SP.Capacitor}, SNode{SP.Inductor}})
                sema_visit_expr!(scope, stmt.val)
            end
            if isa(stmt, SNode{SP.Voltage}) || isa(stmt, SNode{SP.Current})
                for val in stmt.vals
                    sema_visit_expr!(scope, val)
                end
            else
                for param in stmt.params
                    sema_visit_expr!(scope, param.val)
                end
            end
            if isa(stmt, SNode{SP.MOSFET}) || isa(stmt, SNode{SP.VAModelCall})
                push!(scope.exposed_models, LSymbol(stmt.model))
            end
            if isa(stmt, SNode{SP.Resistor})
                if stmt.val !== nothing && (hasparam(stmt.params, "l") || hasparam(stmt.params, "r"))
                    push!(scope.exposed_models, LSymbol(stmt.val))
                end
            end
        elseif isa(stmt, SNode{SP.Model})
            if !isempty(scope.condition_stack)
                error("Model statements not allowed in conditional blocks. If you need this feature, please file an issue.")
            end
            sema_visit_model!(scope, stmt)
        elseif isa(stmt, SNode{SP.Subckt})
            name = LSymbol(stmt.name)
            if haskey(scope.subckts, name)
                warn!(scope, "Duplicate subcircuit definition $name")
            end
            push!(get!(()->[], scope.subckts, name), scope.global_position=>MaybeConditional(scope, sema_file_or_section(stmt; imps=scope.imps, parse_cache=scope.parse_cache, imported_hdl_modules=scope.imported_hdl_modules)))
        elseif isa(stmt, SNode{SP.LibStatement})
            name = LSymbol(stmt.name)
            if haskey(scope.subckts, name)
                warn!(scope, "Duplicate library definition $name")
            end
            # Don't sema this now - it might depend on other sections defined later
            push!(get!(()->[], scope.libs, name), scope.global_position=>SemaSpec(scope, stmt))
        elseif isa(stmt, SNode{SP.OptionStatement})
            for param in stmt.params
                name = LSymbol(param.name)
                push!(get!(()->[], scope.options, name), scope.global_position=>param)
            end
        elseif isa(stmt, SNode{SP.ParamStatement})
            for param in stmt.params
                name = LSymbol(param.name)
                push!(get!(()->[], scope.params, name), scope.global_position=>MaybeConditional(scope, param))
                sema_visit_expr!(scope, param.val)
            end
        elseif isa(stmt, SNode{SP.IncludeStatement}) || isa(stmt, SNode{SP.LibInclude}) || isa(stmt, SNode{SP.HDLStatement})
            if !isempty(scope.condition_stack)
                error("Includes not allowed in conditional blocks. If you need this feature, please file an issue.")
            end
            str = strip(unescape_string(String(stmt.path)), ['"', '\'']) # verify??
            if startswith(str, JLPATH_PREFIX)
                path = str[sizeof(JLPATH_PREFIX)+1:end]
                components = splitpath(path)
                imp = Symbol(components[1])
                imp_mod = sema_resolve_import(scope, imp)
                path = joinpath(components[2:end]...)
                imps = imp_mod
                parse_cache = imp_mod.var"#cedar_parse_cache#"
                imported_hdl_modules=Module[]
            else
                imps = scope.imps
                parse_cache = scope.parse_cache
                imported_hdl_modules = scope.imported_hdl_modules
                if isabspath(str)
                    path = str
                else
                    thispath = scope.ast.ps.srcfile.path
                    if thispath !== nothing
                        path = isabspath(str) ? str : joinpath(dirname(thispath), str)
                        # Otherwise, relative to the cache module (handled by the cache)
                    end
                end
            end
            if isa(stmt, SNode{SP.HDLStatement})
                isempty(scope.condition_stack) || error("HDL includes not allowed in conditional blocks")
                vm = parse_and_cache_va!(parse_cache, path)
                if vm === nothing
                    error("Preprocessing pass missed HDL include $path")
                elseif isa(vm, VANode)
                    error("HDL include $path was not codegen'ed at sema time")
                else
                    push!(scope.imported_hdl_modules, vm[2])
                end
            else
                if parse_cache !== nothing
                    includee = parse_and_cache_spc!(parse_cache, path)
                else
                    includee = SpectreNetlistParser.parsefile(path; implicit_title=false)
                end
                if !isa(includee, SemaResult)
                    includee = sema_file_or_section(includee; imps=imps, parse_cache=parse_cache, imported_hdl_modules)
                    recache_spc!(parse_cache.thismod, path, includee)
                end
                if isa(stmt, SNode{SP.LibInclude})
                    includee = get_section!(includee, LSymbol(stmt.name))
                end
                sema_include!(scope, includee)
            end
        elseif isa(stmt, SNode{SP.Tran}) || isa(stmt, SNode{SP.EndStatement})
            # Ignore for Sema purposes
        elseif isa(stmt, SNode{SP.IfBlock})
            depth = length(scope.condition_stack)
            for case in stmt.cases
                if case.condition === nothing
                    sema!(scope, case)
                else
                    push!(scope.conditionals, scope.global_position=>MaybeConditional(scope, case.condition))
                    cond_id = length(scope.conditionals)
                    push!(scope.condition_stack, cond_id)
                    sema!(scope, case)
                    push!(scope.condition_stack, -cond_id)
                end
            end
            resize!(scope.condition_stack, depth)
        else
            @show stmt
            error()
        end
    end
end

function sema_include!(scope::SemaResult, includee::SemaResult)
    @assert isempty(scope.condition_stack)
    condition_offset = length(scope.conditionals)
    append!(scope.conditionals, includee.conditionals)
    for (name, defs) in includee.params
        namedefs = get!(()->[], scope.params, name)
        for def in defs
            push!(namedefs, offset_cd(def, condition_offset))
        end
    end
    for (name, defs) in includee.models
        namedefs = get!(()->[], scope.models, name)
        for def in defs
            push!(namedefs, offset_cd(def, condition_offset))
        end
    end
    for (name, defs) in includee.subckts
        namedefs = get!(()->[], scope.subckts, name)
        for def in defs
            push!(namedefs, offset_cd(def, condition_offset))
        end
    end
    for (name, defs) in includee.instances
        namedefs = get!(()->[], scope.instances, name)
        for def in defs
            push!(namedefs, offset_cd(def, condition_offset))
        end
    end
    for (name, defs) in includee.libs
        push!(get!(()->[], scope.libs, name), defs...)
    end
    for (name, defs) in includee.options
        push!(get!(()->[], scope.options, name), defs...)
    end
    union!(scope.exposed_parameters, includee.exposed_parameters)
    union!(scope.exposed_models, includee.exposed_models)
    union!(scope.exposed_subckts, includee.exposed_subckts)
end

function sema_file_or_section(n::SNode; imps=nothing, parse_cache=nothing, imported_hdl_modules::Vector{Module}=Module[])
    scope = SemaResult(n)
    if imps !== nothing
        scope.imps = imps
    end
    if parse_cache !== nothing
        scope.parse_cache = parse_cache
    end
    scope.imported_hdl_modules = imported_hdl_modules
    sema!(scope, n)
    scope
end

function sema(n::SNode; imps = nothing, parse_cache=nothing, imported_hdl_modules::Vector{Module}=Module[])
    scope = SemaResult(n)
    if imps !== nothing
        if isa(imps, NamedTuple)
            scope.imps = Dict(pairs(imps)...)
        else
            scope.imps = imps
        end
    end
    if parse_cache !== nothing
        scope.parse_cache = parse_cache
    end
    scope.imported_hdl_modules = imported_hdl_modules
    sema!(scope, n)
    resolve_scopes!(scope)
    scope
end

#=============================== Scope Resultion ==============================#
function resolve_subckt(sema::SemaResult, name::Symbol)
    sema.subckts[name][end][2].val
end

function resolve_scopes!(sr::SemaResult)
    # Go through all models and see if they're from an HDL import instead
    if sr.parse_cache !== nothing
        for (name, modeldefs) in sr.models
            for i = 1:length(modeldefs)
                (pos, cd) = modeldefs[i]
                sa, gr = cd.val
                if gr.mod !== sr.parse_cache.thismod
                    continue
                end

                isdefined(gr.mod, gr.name) && continue
                for imp in sr.imported_hdl_modules
                    if isdefined(imp, gr.name)
                        gr = GlobalRef(imp, gr.name)
                        break
                    end
                end

                if gr.mod === sr.parse_cache.thismod
                    error("Failed to find model $(gr.name)")
                end

                modeldefs[i] = pos=>MaybeConditional(cd.cond, Pair{SNode, GlobalRef}(sa, gr))
            end
        end
    end

    # Go through all subcircuits and resolve them
    for (name, subckts) in sr.subckts
        for i = 1:length(subckts)
            (pos, cd) = subckts[i]
            cd.val.CktID !== nothing && continue
            resolve_scopes!(cd.val)
        end
    end

    # Go through all subcircuit instances, figure out the appropriate definition,
    # and add all implicit parameters/models/subckts to the exposed lists.
    for (_, instances) in sr.instances
        for (_, instance) in instances
            instance = instance.val
            if isa(instance, SNode{SP.SubcktCall})
                subckt = get(sr.subckts, LSymbol(instance.model), nothing)
                if subckt === nothing
                    continue
                end
                subckt = subckt[end][2].val
                for name in subckt.exposed_parameters
                    push!(sr.exposed_parameters, name)
                end
                for name in subckt.exposed_models
                    push!(sr.exposed_models, name)
                end
                # TODO: If a subcircuit is resolved, add its parameters
                for name in subckt.exposed_subckts
                    push!(sr.exposed_subckts, name)
                end
            end
        end
    end

    # Now that we know all the parameters that we need, topologically sort them
    # with our known definitions, both for codegen and to be able to diagnose
    # any cycles.
    graph = SimpleDiGraph(length(sr.params) + length(sr.conditionals)); # vertices are symbol ids in [params; conditionals]
    idx_lookup = Dict{Symbol, Int}(sym=>id for (id, (sym, _)) in enumerate(sr.params))
    conditional(idx) = length(sr.params) + abs(idx)
    for (n, (name, defs)) in enumerate(sr.params)
        def = defs[end]
        cd = def[2]
        if cd.cond != 0
            add_edge!(graph, conditional(cd.cond), n)
        end
        sema_visit_ids!(cd.val.val) do name
            push!(sr.exposed_parameters, name)
            if haskey(idx_lookup, name)
                m = idx_lookup[name]
                if m != n
                    # Self-parameter reference refers to outer scope
                    add_edge!(graph, idx_lookup[name], n)
                end
            end
        end
    end

    for (n, (_, cd)) in enumerate(sr.conditionals)
        if cd.cond != 0
            add_edge!(graph, conditional(cd.cond), conditional(n))
        end
        sema_visit_ids!(cd.val) do name
            push!(sr.exposed_parameters, name)
            if haskey(idx_lookup, name)
                m = idx_lookup[name]
                add_edge!(graph, m, conditional(n))
            end
        end
    end

    try
        sr.parameter_order = topological_sort(graph)
    catch
        @show strongly_connected_components(graph)
    end

    # Figure out which of the parameters we have seen do not have a local
    # definition.
    new_exposed_parameters = OrderedSet{Symbol}()
    new_exposed_models = OrderedSet{Symbol}()
    new_exposed_subckts = OrderedSet{Symbol}()

    provided_binned_models = Set{Symbol}()
    for (model, _) in sr.models
        m = match(binning_rx, String(model))
        if m !== nothing
            push!(provided_binned_models, Symbol(m.captures[1]))
        end
    end

    for param in sr.exposed_parameters
        if !haskey(sr.params, param)
            push!(new_exposed_parameters, param)
        end
    end
    for model in sr.exposed_models
        haskey(sr.models, model) && continue
        (model in provided_binned_models) && continue
        push!(new_exposed_models, model)
    end
    for subckt in sr.exposed_subckts
        if !haskey(sr.subckts, subckt)
            push!(new_exposed_subckts, subckt)
        end
    end

    sr.exposed_parameters = new_exposed_parameters
    sr.exposed_models = new_exposed_models
    sr.exposed_subckts = new_exposed_subckts
end

#================================= ID Assignment ==============================#
function assign_id!(scope::SemaResult, @nospecialize(CktID))
    if scope.CktID !== nothing
        error("Circuit ID already assigned")
    end
    scope.CktID = CktID
end

function sema_assign_ids(r::SemaResult)
    r.CktID !== nothing && return nothing
    s = gensym()
    subs = Expr(:block)
    for (_, sublist) in r.subckts
        for (_, sub) in sublist
            push!(subs.args, sema_assign_ids(sub.val))
        end
    end
    quote
        abstract type $s end
        $(assign_id!)($r, $s)
        (::$(typeof(getsema)))(::Type{$s}) = $r
        $(subs.args...)
        $(SpCircuit){$s, Tuple{}}((;), (;))
    end
end

function sema_codegen_hdl(r::SemaResult)
    ret = Expr(:toplevel)
    for (_, (_, va)) in r.hdl_includes
        push!(ret.args, CedarSim.make_module(va))
    end
    return ret
end
