using StaticArrays

struct CodegenState
    sema::SemaResult
end

LString(s::SNode{<:SP.Terminal}) = lowercase(String(s))
LString(s::SNode{<:SP.AbstractASTNode}) = lowercase(String(s))
LString(s::SNode{<:SC.Terminal}) = String(s)
LString(s::SNode{<:SC.AbstractASTNode}) = String(s)
LString(s::AbstractString) = lowercase(s)
LString(s::Symbol) = lowercase(String(s))
LSymbol(s) = Symbol(LString(s))

function hasparam(params, name)
    for p in params
        if LString(p.name) == name
            return true
        end
    end
    return false
end


function cg_modelref!(state::CodegenState, modelref)

end

function cg_net_name!(state::CodegenState, net)
    return LSymbol(net)
end

function cg_net_name!(state::CodegenState, net::Symbol)
    # *net# is an invalid identifier in both spice and julia, so we can use it without worrying about colliding
    # with spliced expressions
    is_ambiguous(state.sema, net) ? Symbol(string("*net#", net)) : net
end

function cg_model_name!(state::CodegenState, net)
    return LSymbol(net)
end

function cg_model_name!(state::CodegenState, model::Symbol)
    # *net# is an invalid identifier in both spice and julia, so we can use it without worrying about colliding
    # with spliced expressions
    is_ambiguous(state.sema, model) ? Symbol(string("*model#", model)) : model
end

#=============================================== Expressions ===================================================#
"""
    cg_expr!(state, expr::SNode)

Codegen a SPICE or Spectre expression `expr` to julia.
"""
function cg_expr! end

cg_expr!(state::CodegenState, cs::Union{SNode{SP.NumericValue}, SNode{SC.NumericValue}}) = cg_expr!(state, cs.val)
function cg_expr!(state::CodegenState, cs::SNode{SC.FloatLiteral})
    txt = String(cs)
    sf = 1
    if txt[end] ∈ keys(spectre_magnitudes)
        sf = spectre_magnitudes[txt[end]]
        txt = txt[begin:end-1]
    end
    ret = Base.parse(Dec64, txt)
    ret *= sf
    return Float64(ret)
end

using DecFP
const spectre_magnitudes = Dict(
    'T' => d"1e12",
    'G' => d"1e9",
    'M' => d"1e6",
    'K' => d"1e3",
    'k' => d"1e3",
    '_' => d"1",
    '%' => d"1e-2",
    'c' => d"1e-2",
    'm' => d"1e-3",
    'u' => d"1e-6",
    'n' => d"1e-9",
    'p' => d"1e-12",
    'f' => d"1e-15",
    'a' => d"1e-18",
);

const spice_magnitudes = Dict(
    "t" => d"1e12",
    "g" => d"1e9",
    "meg" => d"1e6",
    "k" => d"1e3",
    "m" => d"1e-3",
    "u" => d"1e-6",
    "mil" => d"25.4e-6",
    "n" => d"1e-9",
    "p" => d"1e-12",
    "f" => d"1e-15",
    "a" => d"1e-18",
);
const spice_regex = Regex("($(join(keys(spice_magnitudes), "|")))\$")

const binning_rx = r"(.*)\.([0-9]+)"

function cg_expr!(state::CodegenState, cs::SNode{SP.FloatLiteral})
    txt = lowercase(String(cs))
    sf = 1
    m = match(spice_regex, txt)
    if m !== nothing && haskey(spice_magnitudes, m.match)
        sf = spice_magnitudes[m.match]
        txt = txt[begin:end-length(m.match)]
    end
    ret = Base.parse(Dec64, txt)
    ret *= sf
    return Float64(ret)
end

function cg_expr!(state::CodegenState, cs::Union{SNode{SC.IntLiteral}, SNode{SP.IntLiteral}})
    txt = String(cs)
    Base.parse(Int64, txt)
end

function cg_expr!(state::CodegenState, cs::SNode{SP.JuliaEscape})
    (r, _) = Meta.parse(String(cs.body), 1; raise=false, greedy=true)
    return r
end

function cg_expr!(state::CodegenState, cs::Union{SNode{SC.BinaryExpression}, SNode{SP.BinaryExpression}})
    op = Symbol(cs.op)
    (lhs, rhs) = (cg_expr!(state, cs.lhs), cg_expr!(state, cs.rhs))
    if op == :(||)
        return Expr(:call, (|), lhs, rhs)
    elseif op == :(&&)
        return Expr(:call, (&), lhs, rhs)
    elseif op == Symbol("**")
        return Expr(:call, (^), lhs, rhs)
    elseif op == Symbol("^")
        return Expr(:call, (⊻), lhs, rhs)
    elseif op == Symbol("~^") || op == Symbol("^~")
        return Expr(:call, (~), Expr(:call, (⊻), lhs, rhs))
    else
        return Expr(:call, op, lhs, rhs)
    end
end

function cg_expr!(state::CodegenState, cs::Union{SNode{SC.UnaryOp}, SNode{SP.UnaryOp}})
    op = Symbol(cs.op)
    return Expr(:call, op, cg_expr!(state, cs.operand))
end

function cg_expr!(state::CodegenState, id::Symbol)
    if id == Symbol("true")
        true
    elseif id == Symbol("false")
        false
    elseif id == Symbol("\$time")
        Expr(:call, Symbol("\$time"))
    elseif id == Symbol("temper")
        Expr(:call, Symbol("temper"))
    else
        # TODO: Request parameter generation here.
        id
    end
end

function cg_expr!(state::CodegenState, stmt::Union{SNode{SC.FunctionCall}, SNode{SP.FunctionCall}})
    fname = LSymbol(stmt.id)
    id = lowercase(String(stmt.id))
    if id == "v"
        ckt_nodes = [cg_net_name!(state, n) for n in stmt.args]
        if length(ckt_nodes) == 1
            :($(ckt_nodes[1]).V)
        elseif length(stmt.args) == 2
            :($(ckt_nodes[1]).V - $(ckt_nodes[2]).V)
        end
    elseif isdefined(CedarSim.SpectreEnvironment, Symbol(id))
        args = map(x->cg_expr!(state, x.item), stmt.args)
        Expr(:call, GlobalRef(SpectreEnvironment, Symbol(id)), args...)
    else
        args = map(x->cg_expr!(state, x.item), stmt.args)
        Expr(:call, fname, args...)
    end
end

function cg_expr!(state::CodegenState, cs::Union{SNode{SC.Identifier}, SNode{SP.Identifier}})
    # TODO probably need to disambiguate stuff here
    id = LSymbol(cs)
    return cg_expr!(state, id)
end

cg_expr!(state::CodegenState, n::Union{SNode{SP.Brace}, SNode{SC.Parens}, SNode{SP.Parens}, SNode{SP.Prime}}) = cg_expr!(state, n.inner)

function cg_params!(state::CodegenState, params)
    ret = Expr[]
    for param in params
        push!(ret, Expr(:kw, LSymbol(param.name), cg_expr!(state, param.val)))
    end
    ret
end

"""
    spice_instance(to_julia, ports, name, model, parameters, val=nothing)

Create a spice instance with the given parameters.
This creates a `Named` object using `spicecall`,
and is used for all spice instances except subcircuits.
"""
function cg_spice_instance!(state::CodegenState, ports, name, model, param_exprs)
    port_exprs = map(ports) do port
        cg_net_name!(state, port)
    end
    :($(Named)($model(; $(param_exprs...)), $(LString(name)))($(port_exprs...)))
end

function cg_instance!(state::CodegenState, instance::SNode{SP.SubcktCall})
    ssema = resolve_subckt(state.sema, LSymbol(instance.model))
    params = Expr[Expr(:kw, name, cg_expr!(state, name)) for name in ssema.exposed_parameters]
    passed_parameters = OrderedDict{Symbol, SNode}()
    callee_codegen = CodegenState(ssema)
    ca = :(let; end)
    params = Symbol[]
    for passed_param in instance.params
        name = LSymbol(passed_param)
        # TODO: Pull up dependencies to this level
        def = cg_expr!(callee_codegen, passed_param.val)
        push!(ca.args[end].args, :($name = $def))
        push!(params, name)
    end
    push!(ca.args[end].args, Expr(:tuple, Expr(:parameters, params...)))
    params = ca
    models = Expr(:tuple, Expr(:parameters, [Expr(:kw, name, cg_expr!(state, name)) for name in ssema.exposed_models]...))
    subckts = Expr(:tuple, [resolve_subckt(state.sema, name).CktID for name in ssema.exposed_subckts]...)
    port_exprs = map(instance.nodes) do port
        cg_net_name!(state, port)
    end
    ret = :(SpCircuit{$(ssema.CktID), $subckts}($params, $models)($(port_exprs...)))
    ret
end

function cg_instance!(state::CodegenState, instance::SNode{SP.Resistor})
    # if params contains r or l, val is the model or nothing
    if hasparam(instance.params, "l") || hasparam(instance.params, "r")
        model = GlobalRef(SpectreEnvironment, :resistor)
        if instance.val !== nothing
            model = cg_modelref!(state, modelref)
        end
        return cg_spice_instance!(state, sema_nets(instance), instance.name, model, cg_params!(state, instance.params))
    else
        params = cg_params!(state, instance.params)
        push!(params, Expr(:kw, :r, cg_expr!(state, instance.val)))
        return cg_spice_instance!(state, sema_nets(instance), instance.name, GlobalRef(SpectreEnvironment, :resistor), params)
    end
end

function cg_instance!(state::CodegenState, instance::SNode{SP.Capacitor})
    return cg_spice_instance!(state, sema_nets(instance), instance.name, GlobalRef(SpectreEnvironment, :capacitor), cg_params!(state, instance.params))
end

function cg_instance!(state::CodegenState, instance::SNode{SP.MOSFET})
    return cg_spice_instance!(state, sema_nets(instance), instance.name, cg_model_name!(state, instance.model), cg_params!(state, instance.params))
end

function cg_instance!(state::CodegenState, instance::SNode{<:Union{SP.Voltage, SP.Current}})
    constructor = if instance isa SNode{SP.Voltage}
        GlobalRef(SpectreEnvironment, :vsource)
    elseif instance isa SNode{SP.Current}
        GlobalRef(SpectreEnvironment, :isource)
    else
        error(@show instance)
    end
    # TODO figure out the correct type for the current simulation
    kws = Expr[]
    for val in instance.vals
        if val isa SNode{SP.DCSource}
            jval = cg_expr!(state, val.dcval)
            kw = Expr(:kw, :dc, jval)
            push!(kws, kw)
        elseif val isa SNode{SP.ACSource} # ACSource
            jval = cg_expr!(state, val.acmag)
            kw = Expr(:kw, :ac, jval)
            push!(kws, kw)
        elseif val isa SNode{SP.TranSource} # TranSource
            fname = LSymbol(val.kw)
            fn = getproperty(SpectreEnvironment, fname)
            if fname == :pwl
                # TODO: This isn't the correct module to use
                trancall = Expr(:call, fn, Expr(:macrocall, StaticArrays.var"@SVector", LineNumberNode(val),
                    Expr(:vect, (cg_expr!(state, v) for v in val.values)...)))
            elseif fname == :sin
                trancall = Expr(:call, SpectreEnvironment.spsin, (cg_expr!(state, v) for v in val.values)...)
            else
                trancall = Expr(:call, fn, (cg_expr!(state, v) for v in val.values)...)
            end
            kw = Expr(:kw, :tran, trancall)
            push!(kws, kw)
        else
            @show val
            error("unhandled voltage value $(String(val))")
        end
    end
    return cg_spice_instance!(state, [instance.pos, instance.neg], instance.name, constructor, kws)
end

function devtype_param(model_kind, mosfet_kind)
    if model_kind.name === :bsim4
        return :TYPE => (mosfet_kind == :pmos ? -1 : 1)
    elseif startswith(String(model_kind.name), "bsimcmg")
        return :DEVTYPE => (mosfet_kind == :pmos ? 0 : 1)
    elseif model_kind == :UnimplementedDevice
        # skip
        return nothing
    else
        error("Needs to be filled in per model")
    end
end

function cg_model_def!(state::CodegenState, (model, modelref)::Pair{<:SNode, GlobalRef}, bins::Dict{Symbol, Vector{Symbol}})
    params = Any[]
    typ = LSymbol(model.typ)
    mosfet_type = typ in (:nmos, :pmos) ? typ : nothing
    level = nothing
    version = nothing
    for p in model.parameters
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
        elseif name == :version
            version = parse(Float64, String(p.val))
            continue
        else
            val = cg_expr!(state, p.val)
        end
        push!(params, Expr(:kw, name, Expr(:call, CedarSim.mknondefault, val)))
    end

    # some devices have a version parameter
    # while others have distinct models
    if version !== nothing && modelref.name in (:bsim4,)
        push!(params, Expr(:kw, :version, Expr(:call, CedarSim.mknondefault, version)))
    end

    if mosfet_type !== nothing
        param = devtype_param(modelref, mosfet_type)
        param !== nothing && push!(params, Expr(:kw, param[1], Expr(:call, CedarSim.mknondefault, param[2])))
    end

    m = match(binning_rx, LString(model.name))
    if m !== nothing
        push!(get!(bins, Symbol(m.captures[1]), Vector{Symbol}()),
            LSymbol(model.name))
    end

    lhs = Symbol(LString(model.name))
    return :($(CedarSim.spicecall)($(CedarSim.ParsedModel), $modelref, (;$(params...))))
end

function codegen!(state::CodegenState)
    block = Expr(:block)
    ret = block
    # Codegen simulator options
    if haskey(state.sema.options, :scale) || haskey(state.sema.options, :gmin) || haskey(state.sema.options, :temp)
        block = Expr(:block)
        scale_expr = :(old_options.scale)
        if haskey(state.sema.options, :scale)
            scale_expr = :($(CedarSim.isdefault)(old_options.scale) ? $(cg_expr!(state, state.sema.options[:scale][end][2].val)) : $scale_expr)
        end
        gmin_expr  = :(old_options.gmin)
        if haskey(state.sema.options, :gmin)
            gmin_expr = :($(CedarSim.isdefault)(old_options.gmin) ? $(cg_expr!(state, state.sema.options[:gmin][end][2].val)) : $gmin_expr)
        end
        temp_expr  = :(old_options.temp)
        if haskey(state.sema.options, :temp)
            temp_expr = :($(CedarSim.isdefault)(old_options.temp) ? $(cg_expr!(state, state.sema.options[:temp][end][2].val)) : $temp_expr)
        end
        push!(ret.args, quote
            old_options = $(CedarSim.options)[]
            new_options = $(CedarSim.SimOptions)(; temp = $temp_expr, gmin = $gmin_expr, scale = $scale_expr)
            @Base.ScopedValues.with $(CedarSim.options)=>new_options $block
        end)
    end
    # Codegen nets
    for (net, _) in state.sema.nets
        net_name = cg_net_name!(state, net)
        push!(block.args, :($net_name = net($(QuoteNode(net)))))
    end
    # Implicit and explicit parameters
    if !isempty(state.sema.formal_parameters) || !isempty(state.sema.exposed_parameters)
        push!(block.args, :(var"*params#" = getfield($(Core.Argument(1)), :params)))
    end
    if !isempty(state.sema.exposed_models)
        push!(block.args, :(var"*models#" = getfield($(Core.Argument(1)), :models)))
    end
    for param in state.sema.exposed_parameters
        push!(block.args, :($param = getfield(var"*params#", $(QuoteNode(param)))))
    end
    # Codegen parameter defs
    for (name, defs) in collect(state.sema.params)[state.sema.parameter_order]
        def = cg_expr!(state, defs[end][2].val)
        if name in state.sema.formal_parameters
            push!(block.args, :($name = hasfield(typeof(var"*params#"), $(QuoteNode(name))) ? getfield(var"*params#", $(QuoteNode(name))) : $def))
        else
            push!(block.args, :($name = $def))
        end
    end
    # Implicit and explicit models
    bins = Dict{Symbol, Vector{Symbol}}()
    for model in state.sema.exposed_models
        name = cg_model_name!(state, model)
        push!(block.args, :($name = getfield(var"*models#", $(QuoteNode(model)))))
    end
    for (model, defs) in state.sema.models
        name = cg_model_name!(state, model)
        model_def = cg_model_def!(state, defs[end][2], bins)
        push!(block.args, :($name = $model_def))
    end
    # Binned model aggregation
    for (name, this_bins) in bins
        name = cg_model_name!(state, name)
        push!(block.args, :($name = $(CedarSim.BinnedModel)($(GlobalRef(SpectreEnvironment, :var"$scale"))(), ($(this_bins...),))))
    end
    for (_, (global_pos, instance)) in state.sema.instances
        push!(block.args, LineNumberNode(instance))
        push!(block.args, cg_instance!(state::CodegenState, instance))
    end
    @show ret
    return ret
end

function codegen(scope::SemaResult)
    codegen!(CodegenState(scope))
end
