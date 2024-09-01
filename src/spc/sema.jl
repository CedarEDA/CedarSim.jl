using OrderedCollections
using Graphs
using GF180MCUPDK

const JLPATH_PREFIX = "jlpkg://"

struct ConditionalDef
    cond::SNode
    then::Vector{Pair{UInt, Union{SNode, ConditionalDef}}}
    not::Vector{Pair{UInt, Union{SNode, ConditionalDef}}}
end

@enum CircuitKind begin
    SPICECircuit
    SpectreCircuit
    Mixed
end

struct SemaSpec
    ast::SNode
    # We treat these as lexical
    includepaths::Vector{String}
    pdkincludepaths::Vector{String}
    imps::Union{Nothing, Module, Dict{Symbol, Module}}
end

mutable struct SemaResult
    ast::SNode
    CktID::Union{Nothing, Type}

    title::Union{SNode, Nothing}
    global_position::UInt64
    kind::CircuitKind

    nets::Dict{Symbol, Vector{Pair{UInt, Union{SNode, ConditionalDef}}}}

    params::OrderedDict{Symbol, Vector{Pair{UInt, Union{SNode, ConditionalDef}}}}
    models::Dict{Symbol, Vector{Pair{UInt, Pair{SNode, GlobalRef}}}}
    subckts::Dict{Symbol, Vector{Pair{UInt, Union{SemaResult, ConditionalDef}}}}
    libs::Dict{Symbol, Vector{Union{Pair{UInt, SemaSpec}, Pair{UInt, SemaResult}}}}
    options::Dict{Symbol, Vector{Pair{UInt, SNode}}}

    instances::OrderedDict{Symbol, Pair{UInt, Union{SNode, ConditionalDef}}}

    warnings::Vector

    # While processing sema, all seen parameters/subckts/models. Upon completion,
    # only those that do not have local defs
    exposed_parameters::OrderedSet{Symbol}
    exposed_subckts::OrderedSet{Symbol}
    exposed_models::OrderedSet{Symbol}

    formal_parameters::OrderedSet{Symbol}
    parameter_order::Vector{Int}

    includepaths::Vector{String}
    pdkincludepaths::Vector{String}

    imps::Union{Nothing, Module, Dict{Symbol, Module}}
end

function SemaResult(ast::SNode)
    SemaResult(ast, nothing, nothing, UInt64(0), SpectreCircuit,
        Dict(), Dict(), Dict(), Dict(), Dict(), Dict(), Dict(), Vector(),
        OrderedSet{Symbol}(), OrderedSet{Symbol}(), OrderedSet{Symbol}(),
        OrderedSet{Symbol}(),
        Int[], Vector{String}(), Vector{String}(), nothing)
end

function SemaSpec(scope::SemaResult, ast::SNode)
    SemaSpec(ast, copy(scope.includepaths), copy(scope.pdkincludepaths), scope.imps)
end

const TwoTerminal = Union{SNode{SP.Resistor}, SNode{SP.Inductor}, SNode{SP.Capacitor},
    SNode{SP.Diode}, SNode{SP.Voltage}, SNode{SP.Current}}

const AnySPInstance = Union{SNode{SP.MOSFET}, SNode{SP.SubcktCall},
    SNode{SP.BipolarTransistor}, SNode{SP.Behavioral},
    SNode{SP.CCVS}, SNode{SP.CCCS}, SNode{SP.VCVS}, SNode{SP.VCCS},
    SNode{SP.Switch}, SNode{SP.JuliaDevice}, TwoTerminal}

function warn!(scope::SemaResult, warn)

end

sema_nets(instance::TwoTerminal) = (instance.pos, instance.neg)
sema_nets(instance::SNode{SP.MOSFET}) = (instance.d, instance.g, instance.s, instance.b)
sema_nets(instance::SNode{SP.BipolarTransistor}) = (instance.c, instance.b, instance.e, instance.s)
sema_nets(instance::SNode{SP.SubcktCall}) = instance.nodes

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

sema_visit_ids!(f, expr::Union{SNode{SP.NumericValue}, SNode{SC.NumericValue}}) = nothing
sema_visit_ids!(f, ::Nothing) = nothing

function sema_visit_expr!(scope::SemaResult, @nospecialize(expr))
    sema_visit_ids!(expr) do id
        push!(scope.exposed_parameters, id)
    end
end

function resolve_includepath(path, includepaths, pdkincludepaths=[])
    isfile(path) && return false, path
    for base in includepaths
        fullpath = joinpath(base, path)
        isfile(fullpath) && return false, fullpath
    end
    for base in pdkincludepaths
        fullpath = joinpath(base, path)
        isfile(fullpath) && return true, fullpath
    end
    error("include path $path not found in $includepaths or $pdkincludepaths")
end

function get_section!(scope::SemaResult, name::Symbol)
    if haskey(scope.libs, name)
        sect = scope.libs[name][end]
        if !isa(sect[2], SemaResult)
            spec = sect[2]::SemaSpec
            semad = sema_file_or_section(spec.ast; includepaths=spec.includepaths, imps=spec.imps)
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

function spice_select_device(sema::SemaResult, devkind, level, version, stmt; dialect=:ngspice)
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
                #error("bsim2 not supported")
                #return :bsim2
            elseif level == 8 || level == 49
                #error("bsim3 not supported")
                #return :bsim3
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
        elseif devkind == :sw
            return GlobalRef(CedarSim.SpectreEnvironment, :Switch)
        end
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
    mosfet_type = typ in (:nmos, :pmos) ? typ : nothing
    level = nothing
    version = nothing
    for param in stmt.parameters
        sema_visit_expr!(scope, param.val)

        pname = LSymbol(param.name)
        if pname == :type
            pname = :devtype
            val = LSymbol(param.val)
            @assert val in (:p, :n)
            mosfet_type = val == :p ? :pmos : :nmos
            continue
        elseif pname == :level
            # TODO
            level = Int(parse(Float64, String(param.val)))
            continue
        elseif pname == :version
            version = parse(Float64, String(param.val))
            continue
        end
    end
    modelref = spice_select_device(scope, typ, level, version, stmt)

    push!(get!(()->[], scope.models, name), scope.global_position=>(stmt=>modelref))
    for param in stmt.parameters
        sema_visit_expr!(scope, param.val)
    end
end

function sema!(scope::SemaResult, n::Union{SNode{SPICENetlistSource}, SNode{SP.Subckt}, SNode{SP.LibStatement}})
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
            if haskey(scope.instances, name)
                error("Duplicate instance name $name")
            end
            scope.instances[name] = scope.global_position=>stmt
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
            if isa(stmt, SNode{SP.MOSFET})
                push!(scope.exposed_models, LSymbol(stmt.model))
            end
            if isa(stmt, SNode{SP.Resistor})
                if stmt.val !== nothing && (hasparam(stmt.params, "l") || hasparam(stmt.params, "r"))
                    push!(scope.exposed_models, LSymbol(stmt.val))
                end
            end
        elseif isa(stmt, SNode{SP.Model})
            sema_visit_model!(scope, stmt)
        elseif isa(stmt, SNode{SP.Subckt})
            name = LSymbol(stmt.name)
            if haskey(scope.subckts, name)
                warn!(scope, "Duplicate subcircuit definition $name")
            end
            push!(get!(()->[], scope.subckts, name), scope.global_position=>sema(stmt; includepaths=scope.includepaths, imps=scope.imps))
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
                push!(get!(()->[], scope.params, name), scope.global_position=>param)
                sema_visit_expr!(scope, param.val)
            end
        elseif isa(stmt, SNode{SP.IncludeStatement}) || isa(stmt, SNode{SP.LibInclude})
            str = strip(unescape_string(String(stmt.path)), ['"', '\'']) # verify??
            if startswith(str, JLPATH_PREFIX)
                path = str[sizeof(JLPATH_PREFIX)+1:end]
                components = splitpath(path)
                imp = Symbol(components[1])
                imp_mod = sema_resolve_import(scope, imp)
                path = joinpath(dirname(pathof(imp_mod)), "..", components[2:end]...)
                imps = imp_mod
            else
                ispdk, path = resolve_includepath(str, scope.includepaths, scope.pdkincludepaths)
                imps = scope.imps
            end
            sa = SP.parsefile(path; implicit_title=false)
            includee = sema_file_or_section(sa; includepaths=[dirname(path); scope.includepaths], imps=imps)
            if isa(stmt, SNode{SP.LibInclude})
                includee = get_section!(includee, LSymbol(stmt.name))
            end
            sema_include!(scope, includee)
        elseif isa(stmt, SNode{SP.Tran}) || isa(stmt, SNode{SP.EndStatement})
            # Ignore for Sema purposes
        else
            @show stmt
            error()
        end
    end
end

function sema_include!(scope::SemaResult, includee::SemaResult)
    for (name, defs) in includee.params
        push!(get!(()->[], scope.params, name), defs...)
    end
    for (name, defs) in includee.models
        push!(get!(()->[], scope.models, name), defs...)
    end
    for (name, defs) in includee.subckts
        push!(get!(()->[], scope.subckts, name), defs...)
    end
    for (name, defs) in includee.instances
        push!(get!(()->[], scope.instances, name), defs...)
    end
    for (name, defs) in includee.libs
        push!(get!(()->[], scope.libs, name), defs...)
    end
    for (name, defs) in includee.options
        push!(get!(()->[], scope.options, name), defs...)
    end
end

function sema_file_or_section(n::SNode; includepaths=nothing, imps=nothing)
    scope = SemaResult(n)
    if includepaths !== nothing
        scope.includepaths = includepaths
    end
    if imps !== nothing
        scope.imps = imps
    end
    sema!(scope, n)
    scope
end

function sema(n::SNode; includepaths=nothing, imps = nothing)
    scope = SemaResult(n)
    if includepaths !== nothing
        scope.includepaths = includepaths
    end
    if imps !== nothing
        if isa(imps, NamedTuple)
            scope.imps = Dict(pairs(imps)...)
        else
            scope.imps = imps
        end
    end
    sema!(scope, n)
    resolve_scopes!(scope)
    scope
end

#=============================== Scope Resultion ==============================#
function resolve_subckt(sema::SemaResult, name::Symbol)
    sema.subckts[name][end][2]
end

function resolve_scopes!(sr::SemaResult)
    # Go through all subcircuit instances, figure out the appropriate definition,
    # and add all implicit parameters/models/subckts to the exposed lists.
    for (_, (_, instance)) in sr.instances
        if isa(instance, SNode{SP.SubcktCall})
            subckt = get(sr.subckts, LSymbol(instance.model), nothing)
            if subckt === nothing
                continue
            end
            subckt = subckt[end][2]
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

    # Now that we know all the parameters that we need, topologically sort them
    # with our known definitions, both for codegen and to be able to diagnose
    # any cycles.
    graph = SimpleDiGraph(length(sr.params)); # vertices are symbol ids in params
    idx_lookup = Dict{Symbol, Int}(sym=>id for (id, (sym, _)) in enumerate(sr.params))
    for (n, (name, defs)) in enumerate(sr.params)
        def = defs[end]
        sema_visit_ids!(def[2].val) do name
            push!(sr.exposed_parameters, name)
            if haskey(idx_lookup, name)
                add_edge!(graph, idx_lookup[name], n)
            end
        end
    end

    sr.parameter_order = topological_sort(graph)

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
    s = gensym()
    subs = Expr(:block)
    for (_, sublist) in r.subckts
        for (_, sub) in sublist
            push!(subs.args, sema_assign_ids(sub))
        end
    end
    quote
        abstract type $s end
        $(assign_id!)($r, $s)
        (::$(typeof(getsema)))(::Type{$s}) = $r
        $(subs.args...)
        SpCircuit{$s, Tuple{}}((;), (;))
    end
end
