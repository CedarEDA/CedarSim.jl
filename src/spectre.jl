using SpectreNetlistParser
using SpectreNetlistParser: SpectreNetlistCSTParser, SPICENetlistParser
using .SPICENetlistParser: SPICENetlistCSTParser
using .SpectreNetlistCSTParser:
    SpectreNetlistSource
using .SPICENetlistCSTParser:
    SPICENetlistSource
using Base.Meta
using StaticArrays
using DecFP

const SNode = SpectreNetlistCSTParser.Node

const SC = SpectreNetlistCSTParser
const SP = SPICENetlistCSTParser

LString(s::SNode{<:SP.Terminal}) = lowercase(String(s))
LString(s::SNode{<:SP.AbstractASTNode}) = lowercase(String(s))
LString(s::SNode{<:SC.Terminal}) = String(s)
LString(s::SNode{<:SC.AbstractASTNode}) = String(s)
LString(s::AbstractString) = lowercase(s)
LString(s::Symbol) = lowercase(String(s))
LSymbol(s) = Symbol(LString(s))

function Base.LineNumberNode(n::SNode)
    sf = n.ps.srcfile
    lsf = sf.lineinfo
    lno_first = SpectreNetlistParser.LineNumbers.compute_line(lsf, n.startof+n.expr.off)
    LineNumberNode(lno_first, Symbol(sf.path))
end


struct SpcScope
    nets::Dict{Symbol, Expr}
    # parameters and their defaults declared in this scope
    params::Dict{Symbol, Any}
    # static parameters that can't be overridden by the user
    sparams::Dict{Symbol, Any}
    # unresolved variables exposed as implicit parameters (dynamic scope)
    variables::Set{Symbol}
    includepaths::Vector{String}
    # includes that can't be parameterized
    pdkincludepaths::Vector{String}
    parsed_files::Dict{String, Any}
    libraries::Set{Tuple{String,String}}
    # models and circuits that a PDK exports
    exports::Set{Symbol}
    # these two variables have similar but different semantics
    # global_scope means "can we define constants, structs, and macros here"
    # is_circuit means "is this code in the context of a (sub)circuit as opposed to a netlist"
    # for example a netlist inside a custom function is not a circuit and not global
    # but a subcircuit in a toplevel netlist is a circuit and not global
    global_scope::Bool
    is_circuit::Bool
end
function SpcScope(global_scope::Bool = false, is_circuit::Bool = true)
    SpcScope(Dict(), Dict(), Dict(), Set(), [], [], Dict(), Set(), Set(), global_scope, is_circuit)
end

abstract type AbstractParamLens end

setproperties(obj, nt::@NamedTuple{}) = obj
@generated function setproperties(obj, nt::NamedTuple)
    T = obj
    values = Expr[]
    for fieldname in fieldnames(T)
        if fieldname in fieldnames(nt)
            push!(values, :(nt.$fieldname))
        else
            push!(values, :(obj.$fieldname))
        end
    end
    return :($(T.name.wrapper)($(values...)))
end
function setproperties(obj; kw...)
    setproperties(obj, (;kw...))
end

@generated function canonicalize_params(nt::NamedTuple)
    par = []
    ch = []
    for p in fieldnames(nt)
        if p == :params
            append!(par, [:($pp=nt.params.$pp) for pp in fieldnames(fieldtype(nt, :params))])
        elseif fieldtype(nt, p) <: Number
            push!(par, :($p=nt.$p))
        elseif fieldtype(nt, p) <: NamedTuple
            push!(ch, :($p=canonicalize_params(nt.$p)))
        end
    end
    return quote
        (; params=(;$(par...)), $(ch...))
    end
end

function canonicalize_params(p::Dict)
    res = empty(p)
    for (k, v) in p
        if v isa Dict
            res[k] = canonicalize_params(v)
        elseif v isa NamedTuple
            res[k] = canonicalize_params(Dict(pairs(v)))
        elseif v isa Number
            get!(res, :params, Dict{Symbol, Number}())[k] = v
        end
    end
    res
end

@generated function compact_params(nt::NamedTuple)
    par = []
    ch = []
    for p in fieldnames(nt)
        if p == :params
            for pp in fieldnames(fieldtype(nt, :params))
                if pp in fieldnames(nt)
                    push!(par, :($pp=nt.params.$pp))
                else
                    push!(ch, :($pp=nt.params.$pp))
                end
            end
        elseif fieldtype(nt, p) <: Number
            push!(ch, :($p=nt.$p))
        elseif fieldtype(nt, p) <: NamedTuple
            push!(ch, :($p=compact_params(nt.$p)))
        end
    end
    if isempty(par)
        return quote
            (; $(ch...))
        end
    else
        return quote
            (; params=(;$(par...)), $(ch...))
        end
    end
end

struct IdentityLens <: AbstractParamLens; end
Base.getproperty(lens::IdentityLens, ::Symbol; type=:unknown) = lens
(::IdentityLens)(;kwargs...) = values(kwargs)
(::IdentityLens)(val) = val

struct ValLens{T} <: AbstractParamLens
    val::T
end
Base.getproperty(lens::ValLens, ::Symbol; type=:unknown) = cedarerror("Reached terminal lens")
(lens::ValLens)(val) = getfield(lens, :val)

"""
    ParamLens(::NamedTuple)

Takes a nested named tuple to override arguments.
For example `sweep.foo(bar=1)` by default returns `(bar=1,)`
unless `ParamLens((foo=(bar=2,),))` is used, in which case it'll return `(bar=2,)`
"""
struct ParamLens{NT<:NamedTuple} <: AbstractParamLens
    nt::NT
    function ParamLens(nt::NT=(;)) where {NT<:NamedTuple}
        #nnt = canonicalize_params(nt)
        new{typeof(nt)}(nt)
    end
end

function Base.getproperty(🔍::ParamLens{T}, sym::Symbol; type=:unknown) where T
    nt = getfield(🔍, :nt)
    nnt = get(nt, sym, (;))
    if !isa(nnt, NamedTuple)
        return ValLens(nnt)
    end
    isempty(nnt) && return IdentityLens()
    return ParamLens(nnt)
end

function (🔍::ParamLens)(;kwargs...)
    nt = getfield(🔍, :nt)
    hasfield(typeof(nt), :params) || return values(kwargs)
    merge(values(kwargs), nt.params)
end

(🔍::ParamLens{typeof((;))})(val) = val
@generated function (🔍::AbstractParamLens)(val)
    isprimitivetype(val) && error("Should have reached trivial lens before this point")
    Expr(:new, :(typeof(val)), (:(getproperty(🔍, $(QuoteNode(name)))(getfield(val, $(QuoteNode(name))))) for name in fieldnames(val))...)
end

struct ApplyLens{T}
    circuit::T
    ApplyLens(circuit) = new{Core.Typeof(circuit)}(circuit)
end
(apply::ApplyLens)(🔍::AbstractParamLens) = (🔍(apply.circuit()))()

function ParamSim(circuit::Type, mode, spec, params)
    return ParamSim(ApplyLens(circuit), mode, spec, params)
end

"""
    ParamObserver()

An "observer" lens that, when passed to a circuit, collects the hierarchy
of requested parameters and their default values.
"""
struct ParamObserver <: AbstractParamLens
    name::Symbol
    type::Any
    params::Dict{Symbol, Any}
end
ParamObserver(name=:top, type=nothing; kwargs...) = ParamObserver(name, type, canonicalize_params(Dict{Symbol, Any}(kwargs...)))

function Base.propertynames(👀::ParamObserver)
    return [fieldnames(ParamObserver)..., keys(getfield(👀, :params))...]
end

function Base.getproperty(👀::ParamObserver, sym::Symbol; type=nothing)
    # unlike the lens, this allows access to properties as well
    if sym == :params
        return NamedTuple(getfield(👀, :params)[:params])
    elseif hasfield(ParamObserver, sym)
        return getfield(👀, sym)
    end
    # By default, look up a subcircuit
    dict = getfield(👀, :params)
    get!(dict, sym, ParamObserver(sym, type))
end

function (👀::ParamObserver)(;kwargs...)
    # Look up a set of local parameters
    dict = get!(getfield(👀, :params), :params, Dict{Symbol, Number}())
    for (param, value) in kwargs
        get!(dict, param, value)
    end
    return (; (k=>dict[k] for k in keys(kwargs))...)
end

function Base.show(io::IO, ::MIME"text/plain", 👀::ParamObserver; indent=0)
    print(io, "(ParamObserver) $(getfield(👀, :name))::$(getfield(👀, :type)) ")
    pretty_print(io, getfield(👀, :params), 0)
    print(io, "\n")
end

make_nt(x) = x
make_nt(x::ParamObserver) = make_nt(getfield(x, :params))
make_nt(dict::Dict) = (; (k => make_nt(v) for (k, v) in dict)...)
function Base.convert(to::Type{NamedTuple}, 👀::ParamObserver)
    return compact_params(make_nt(👀))
end

function pretty_print(io::IO, d::Dict, indent = 0; Δindent = 4)
    outerpadding = " " ^ (indent)
    padding = " " ^ (indent + Δindent)
    println(io, "(;")
    for (k,v) in sort(d; by=x->x==:params, rev=true)
        print(io,  padding * string(k) * " = ")
        if v isa ParamObserver
            print(io, "(ParamObserver) $(getfield(v, :name))::$(nameof(getfield(v, :type))) ")
            pretty_print(io, getfield(v, :params), indent + Δindent)
        elseif v isa Dict
            pretty_print(io, v, indent + Δindent)
        else
            print(io, v)
        end
        println(io, ",")
    end
    print(io, outerpadding * ")")
    return nothing
end

macro param(path)
    esc(Expr(:., Expr(:., path.args[1], QuoteNode(:params)), path.args[2]))
end

export @param

function fieldvalues(x::T) where {T}
     !isstructtype(T) && throw(ArgumentError("$(T) is not a struct type"))

     return ((CedarSim.undefault(getfield(x, name)) for name in fieldnames(T))...,)
end

function ntfromstruct(x::T) where {T}
     !isstructtype(T) && throw(ArgumentError("$(T) is not a struct type"))
     names = fieldnames(T)
     values = fieldvalues(x)
     return NamedTuple{names}(values)
end
ntfromstruct(x::CedarSim.ParallelInstances) = (; m=x.multiplier, ntfromstruct(x.device)...)

function modelparams(m)
    args = m.params
    t = m.type
    i = spicecall(t; NamedTuple(args)...)
    ntfromstruct(i)
end

noiseparams(circ::AbstractSim) = noiseparams(circ.circuit)
function noiseparams(circ)
    observer = ParamObserver()
    circ(observer)
    noiseparams(observer)
end
function noiseparams(👀::ParamObserver)
    t = something(getfield(👀, :type), Nothing)
    noisefields = filter((fn -> startswith(String(fn), "ϵ"), modelfields(typeof(t)))...)
    args = getfield(👀, :params)
    childfields = []
    for (k, v) in args
        if typeof(v) <: ParamObserver
            fields = noiseparams(v)
            if !isempty(fields)
                push!(childfields, (k => fields))
            end
        end
    end
    (; (f => 0.0 for f in noisefields)...,
        childfields...)
end

# source: https://rosettacode.org/wiki/Topological_sort#Julia
function toposort(data::Dict{T,Set{T}}) where T
    selfdeps = Set{T}()
    for (k, v) in data
        if k ∈ v
            push!(selfdeps, k)
            delete!(v, k)
        end
    end
    extdeps = setdiff(reduce(∪, values(data); init=Set{T}()), keys(data))
    for item in extdeps
        data[item] = Set{T}()
    end
    rst = Vector{T}()
    while true
        ordered = Set(item for (item, dep) in data if isempty(dep))
        if isempty(ordered) break end
        append!(rst, ordered)
        data = Dict{T,Set{T}}(item => setdiff(dep, ordered) for (item, dep) in data if item ∉ ordered)
    end
    isempty(data) || cedarerror("a cyclic dependency exists amongst $(data)")
    # items that depend on external items or on themselves
    # (meaning, on the same item in the parent scope)
    extraitems = union(selfdeps, extdeps)
    return extraitems, rst
end

"""
    dependencies(::Expr)

scan expr for @ckt_ macrocalls and return a list of the macro names
"""
function dependencies(e)
    deps = Set{Symbol}()
    if e isa Expr
        if (e.head == :macrocall &&
            e.args[1] isa Symbol &&
            startswith(String(e.args[1]), "@ckt_"))
            push!(deps, e.args[1])
        else
            for a in e.args
                union!(deps, dependencies(a))
            end
        end
    end
    return deps
end

"""
    sort_ckts(ckts::Dict{Symbol, Expr})

Sort circuits by dependencies.
SPICE is a multi-pass parser and allows circuits to be defined out of order.
In order for macro expansion to work, we need to topologically sort the circuits.
"""
function sort_ckts(ckts::Dict{Symbol, Expr})
    deps = Dict{Symbol, Set{Symbol}}()
    for (name, ckt) in ckts
        deps[name] = dependencies(ckt)
    end
    extra, sorted = toposort(deps)
    isempty(extra) || @warn("Possibly unresolved dependencies: $extra")
    return [get(ckts, name, nothing) for name in sorted]
end

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

function (to_julia::SpcScope)(n::SNode{T}) where T
    println(T) # show the inner type, which is not shown in the stack trace
    throw(MethodError(to_julia, (n,)))
end

function (to_julia::SpcScope)(n)
    n # already a Julia type
end

function (to_julia::SpcScope)(cs::SNode{SC.NumericValue})
    to_julia(cs.val)
end

function (to_julia::SpcScope)(cs::SNode{SP.NumericValue})
    to_julia(cs.val)
end

function (::SpcScope)(cs::SNode{SC.FloatLiteral})
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

function (::SpcScope)(cs::SNode{SP.FloatLiteral})
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

function (::SpcScope)(cs::Union{SNode{SC.IntLiteral}, SNode{SP.IntLiteral}})
    txt = String(cs)
    Base.parse(Int64, txt)
end

function (::SpcScope)(cs::SNode{SP.JuliaEscape})
    (r, _) = Meta.parse(String(cs.body), 1; raise=false, greedy=true)
    return r
end

function (::SpcScope)(ll::SNode{SP.Literal})
    @show ll
    error()
end

function (to_julia::SpcScope)(cs::Union{SNode{SC.BinaryExpression}, SNode{SP.BinaryExpression}})
    op = Symbol(cs.op)
    if op == :(||)
        return Expr(:call, (|), to_julia(cs.lhs), to_julia(cs.rhs))
    elseif op == :(&&)
        return Expr(:call, (&), to_julia(cs.lhs), to_julia(cs.rhs))
    elseif op == Symbol("**")
        return Expr(:call, (^), to_julia(cs.lhs), to_julia(cs.rhs))
    elseif op == Symbol("^")
        return Expr(:call, (⊻), to_julia(cs.lhs), to_julia(cs.rhs))
    elseif op == Symbol("~^") || op == Symbol("^~")
        return Expr(:call, (~), Expr(:call, (⊻), to_julia(cs.lhs), to_julia(cs.rhs)))
    else
        return Expr(:call, op, to_julia(cs.lhs), to_julia(cs.rhs))
    end
end

function (to_julia::SpcScope)(cs::Union{SNode{SC.UnaryOp}, SNode{SP.UnaryOp}})
    op = Symbol(cs.op)
    return Expr(:call, op, to_julia(cs.operand))
end

function (to_julia::SpcScope)(cs::Union{SNode{SC.Identifier}, SNode{SP.Identifier}})
    # TODO probably need to disambiguate stuff here
    id = LSymbol(cs)
    if id == Symbol("true")
        true
    elseif id == Symbol("false")
        false
    elseif id == Symbol("\$time")
        Expr(:call, Symbol("\$time"))
    elseif id == Symbol("temper")
        Expr(:call, Symbol("temper"))
    else
        # if the id isn't provided by a parameter or SpectreEnvironment
        # we need to propagate it to the caller
        if !haskey(to_julia.params, id) && !hasproperty(SpectreEnvironment, id)
            push!(to_julia.variables, id)
        end
        id
    end
end

function (to_julia::SpcScope)(cs::Union{SNode{SC.Parameters}, SNode{SP.ParamStatement}})
    for par in cs.params
        id = LSymbol(par.name)
        if to_julia.is_circuit
            to_julia.params[id] = par.val
        else
            to_julia.sparams[id] = par.val
        end
        delete!(to_julia.variables, id)
    end
end

function (to_julia::SpcScope)(stmt::Union{SNode{SC.Parens}, SNode{SP.Parens}, SNode{SP.Prime}})
    return to_julia(stmt.inner)
end

function (to_julia::SpcScope)(stmt::Union{SNode{SC.FunctionCall}, SNode{SP.FunctionCall}})
    fname = LSymbol(stmt.id)
    id = lowercase(String(stmt.id))
    if id == "v"
        ckt_nodes = [Symbol(string("node_", LString(n.item))) for n in stmt.args]
        if length(ckt_nodes) == 1
            :($(ckt_nodes[1]).V)
        elseif length(stmt.args) == 2
            :($(ckt_nodes[1]).V - $(ckt_nodes[2]).V)
        end
    elseif isdefined(SpectreEnvironment, Symbol(id))
        args = map(x->to_julia(x.item), stmt.args)
        Expr(:call, GlobalRef(SpectreEnvironment, Symbol(id)), args...)
    else
        args = map(x->to_julia(x.item), stmt.args)
        Expr(:call, fname, args...)
    end
end

function (to_julia::SpcScope)(cs::Union{SNode{SC.TernaryExpr}, SNode{SP.TernaryExpr}})
    return Expr(:if, to_julia(cs.condition), to_julia(cs.ifcase), to_julia(cs.elsecase))
end

function (to_julia::SpcScope)(n::SNode{SC.Model})
    params = Any[]
    for p in n.params
        name = Symbol(uppercase(String(p.name)))
        if name == :TYPE
            name = :DEVTYPE
            val = Symbol(p.val)
            @assert val in (:p, :n)
            val = val == :p ? 0 : 1
        elseif name == :LEVEL
            # TODO
            continue
        else
            val = to_julia(p.val)
        end
        push!(params, Expr(:kw, name, Expr(:call, mknondefault, val)))
    end
    lhs = Symbol(n.name)
    rhs = :($ParsedModel($(Symbol(n.master_name)), (;$(params...))))
    if to_julia.global_scope
        :(const $lhs = $rhs)
    else
        quote
            @provides $lhs
            $lhs = $rhs
        end
    end
end

function (to_julia::SpcScope)(n::SNode{SP.Brace})
    return to_julia(n.inner)
end

function spice_select_device(devkind, level, version, stmt; dialect=:ngspice)
    if devkind == :d
        return :(SpectreEnvironment.diode)
    elseif devkind == :r
        return :(SpectreEnvironment.resistor)
    elseif devkind == :c
        return :(SpectreEnvironment.capacitor)
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
                return :bsim4
            elseif level == 17 || level == 72
                if version == 107 || version === nothing
                    return :bsimcmg107
                else
                    file = stmt.ps.srcfile.path
                    line = SpectreNetlistParser.LineNumbers.compute_line(stmt.ps.srcfile.lineinfo, stmt.startof)
                    @warn "Version $version of mosfet $devkind at level $level not implemented" _file=file _line=line
                    return :UnimplementedDevice
                end
            else
                file = stmt.ps.srcfile.path
                line = SpectreNetlistParser.LineNumbers.compute_line(stmt.ps.srcfile.lineinfo, stmt.startof)
                @warn "Mosfet $devkind at level $level not implemented" _file=file _line=line
                return :UnimplementedDevice
            end
        elseif devkind == :sw
            return :(SpectreEnvironment.Switch)
        end
    end
    file = stmt.ps.srcfile.path
    line = SpectreNetlistParser.LineNumbers.compute_line(stmt.ps.srcfile.lineinfo, stmt.startof)
    @warn "Device $devkind at level $level not implemented" _file=file _line=line
    return :UnimplementedDevice
end

function devtype_param(model_kind, mosfet_kind)
    if model_kind == :bsim4
        return :TYPE => (mosfet_kind == :pmos ? -1 : 1)
    elseif startswith(String(model_kind), "bsimcmg")
        return :DEVTYPE => (mosfet_kind == :pmos ? 0 : 1)
    elseif model_kind == :UnimplementedDevice
        # skip
        return nothing
    else
        error("Needs to be filled in per model")
    end
end

function (to_julia::SpcScope)(n::SNode{SC.SpectreArray})
    Expr(:macrocall, StaticArrays.var"@SVector", LineNumberNode(n),
        Expr(:vect, (to_julia(v) for v in n.items)...))
end

function (to_julia::SpcScope)(n::Union{SNode{SC.Options}, SNode{SP.OptionStatement}})
    for p in n.params
        name = LString(p.name)
        val = something(p.val, true)
        if  (val isa SNode{SP.Identifier}
          || val isa SNode{SC.Identifier}
          || val isa SNode{SC.Literal})
            # we only support numerical options at the moment
            continue
        end
        param = Symbol("option_", name)
        if to_julia.is_circuit
            to_julia.params[param] = val
        else
            to_julia.sparams[param] = val
        end
    end
end

function (to_julia::SpcScope)(n::SNode{SP.TempStatement})
    if to_julia.is_circuit
        to_julia.params[:option_temp] = n.temp
    else
        to_julia.sparams[:option_temp] = n.temp
    end
end

const binning_rx = r"(.*)\.([0-9]+)"

function (to_julia::SpcScope)(n::SNode{SP.Model}, bins::Dict{Symbol, Vector{Symbol}})
    params = Any[]
    typ = LSymbol(n.typ)
    mosfet_type = typ in (:nmos, :pmos) ? typ : nothing
    level = nothing
    version = nothing
    for p in n.parameters
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
            val = to_julia(p.val)
        end
        push!(params, Expr(:kw, name, Expr(:call, mknondefault, val)))
    end
    dev = spice_select_device(typ, level, version, n)

    # some devices have a version parameter
    # while others have distinct models
    if version !== nothing && dev in (:bsim4,)
        push!(params, Expr(:kw, :version, Expr(:call, mknondefault, version)))
    end

    if mosfet_type !== nothing
        param = devtype_param(dev, mosfet_type)
        param !== nothing && push!(params, Expr(:kw, param[1], Expr(:call, mknondefault, param[2])))
    end

    m = match(binning_rx, LString(n.name))
    if m !== nothing
        push!(get!(bins, Symbol(m.captures[1]), Vector{Symbol}()),
            LSymbol(n.name))
    end

    lhs = Symbol(LString(n.name))
    rhs = :($spicecall($ParsedModel, $dev, (;$(params...))))
    push!(to_julia.exports, lhs)

    if to_julia.global_scope
        :(const $lhs = $rhs)
    else
        quote
            @provides $lhs
            $lhs = $rhs
        end
    end
end

function get_net!(to_julia::SpcScope, net_name::String)
    ns = Symbol("node_", net_name)
    if ns ∉ keys(to_julia.nets)
        if net_name in ("0", "gnd")
            to_julia.nets[ns] = quote
                $ns = net($(String(ns)))
                $(Named)(Gnd(), "gnd")($ns)
            end
        else
            to_julia.nets[ns] = :($ns = net($(String(ns))))
        end
    end
    ns
end

macro isckt_or(name, m, default)
    @assert Base.isexpr(m, :macrocall)
    try
        # try to resolve the macrocall
        Base.eval(__module__, m.args[1])
    catch
        return esc(default)
    end
    return esc(:(@subckt($name, $(macroexpand(__module__, m; recursive=false)))))
end

function (to_julia::SpcScope)(stmt::SNode{SC.Instance})
    nets = map(stmt.nodelist.nodes) do node
        @assert isempty(node.subckts)
        get_net!(to_julia, String(node.node))
    end

    circsym = Symbol(string("@ckt_", String(stmt.master)))
    mcall = Expr(:macrocall, circsym,
        LineNumberNode(stmt),
        to_julia.is_circuit ?
            :(getproperty(🔍, $(QuoteNode(Symbol(stmt.name))); type=$(circsym))) :
            ParamLens((;)),
        :($(nets...),))
    for par in stmt.params
        id = Symbol(par.name)
        val = to_julia(par.val)
        kw = Expr(:(=), id, val)
        push!(mcall.args, kw)
    end

    fsym = Symbol(stmt.master)
    #TODO determine correct casing, spicecall is incorrect here
    call = Expr(:call, spicecall, fsym)
    if to_julia.is_circuit
        call = Expr(:call, :(getproperty(🔍, $(QuoteNode(Symbol(stmt.name))); type=$(fsym))))
        outer_call = :($spicecall($fsym; $call...))
    else
        call = outer_call = :($spicecall($fsym))
    end
    for par in stmt.params
        id = Symbol(par.name)
        valex = to_julia(par.val)
        if id === :wave
            kw = Expr(:kw, :tran, :(pwl($valex)))
        elseif id === :type
            # TODO: validate that the type matches the other params
            #       (e.g. no `wave=...` unless `type=pwl` provided)
            continue
        else
            kw = Expr(:kw, id, valex)
        end
        push!(call.args, kw)
    end
    if hasproperty(SpectreEnvironment, fsym)
        :($(Named)($outer_call, $(String(stmt.name)))($(nets...)))
    else
        fquote = quote
            @requires $fsym
            $(Named)($outer_call, $(String(stmt.name)))($(nets...))
        end
        :(@isckt_or $(Symbol(stmt.name)) $mcall $fquote)
    end
end

"""
    spice_instance(to_julia, ports, name, model, parameters, val=nothing)

Create a spice instance with the given parameters.
This creates a `Named` object using `spicecall`,
and is used for all spice instances except subcircuits.
"""
function spice_instance(to_julia, ports, name, model, parameters; kwargs...)
    nets = map(ports) do node
        get_net!(to_julia, LString(node.name))
    end

    fsym = model
    if !isa(fsym, Union{Symbol, GlobalRef})
        fsym = Symbol(lowercase(String(model)))
    end
    if to_julia.is_circuit
        call = Expr(:call, :(getproperty(🔍, $(QuoteNode(LSymbol(name))); type=$(fsym))))
        outer_call = :($spicecall($fsym; $call...))
    else
        call = outer_call = :($spicecall($fsym))
    end
    for (id, val) in kwargs
        if val !== nothing
            val = to_julia(val)
            kw = Expr(:kw, id, val)
            push!(call.args, kw)
        end
    end
    if isa(parameters, Vector{Expr})
        append!(call.args, parameters)
    else
        for par in parameters
            id = LSymbol(par.name)
            val = to_julia(par.val)
            kw = Expr(:kw, id, val)
            push!(call.args, kw)
        end
    end
    call = :($(Named)($outer_call, $(LString(name)))($(nets...)))
    if isa(fsym, GlobalRef)
        call
    else
        quote
            @requires $fsym
            $call
        end
    end
end

function (to_julia::SpcScope)(stmt::SNode{SP.JuliaDevice})
    nets = map(stmt.nodes) do node
        get_net!(to_julia, LString(node.name))
    end
    expr = to_julia(stmt.dev)
    return :($(Named)($expr, $(LString(stmt.name)))($(nets...)))
end

"""
    subckt_instance(to_julia, ports, name, model, parameters)

Create a subcircuit instance with the given parameters.
This is like `spice_instance`, but uses a macro call to create the instance.
It also takes care to peel off a layer of the parameter lens.
"""
function subckt_instance(to_julia, ports, name, model, parameters)
    nets = map(ports) do node
        get_net!(to_julia, LString(node.name))
    end

    fsym = Symbol("@ckt_", lowercase(String(model)))
    lnn = name === nothing ? LineNumberNode(@__LINE__, @__FILE__) : LineNumberNode(name)
    call = Expr(:macrocall, fsym, lnn,
        to_julia.is_circuit ?
            (name === nothing ? :🔍 : :(getproperty(🔍, $(QuoteNode(LSymbol(name))); type=$(fsym)))) :
            ParamLens((;)),
        :($(nets...),))
    # parameters can refer to other parameters so we need to make sure they are added to the params locally
    # this is slightly incorrect since in reality parameters are topologically sorted
    # this would be fixed by https://github.com/JuliaComputing/DynamicScope.jl/issues/1
    let_to_julia = setproperties(to_julia, params=copy(to_julia.params))
    for par in parameters
        id = LSymbol(par.name)
        val = let_to_julia(par.val)
        kw = Expr(:(=), id, val)
        let_to_julia.params[id] = val
        push!(call.args, kw)
    end
    if name === nothing
        call
    else
        Expr(:macrocall, var"@subckt", lnn, LSymbol(name), call)
    end
end

function net_alias(net, name)
    observed!(net.V, DScope(debug_scope[], name))
end

"""
    subckt_macro(to_julia, name, ckt_nodes, body)

Creates a subcircuit definition.
This uses the variables and parameters collected on the SpcScope
to create function parameters and an appropriate `@requires` statement.

It then creates a @dyn function that applies the parameter lens, and returns a closure.

In SPICE parameters can be defined out of order.
For this reason we collect the dependencies of each param,
and then sort them topologically.
"""
function subckt_macro(to_julia, name, ckt_nodes, body)
    fsym = Symbol("ckt_", lowercase(String(name)))
    deps = Dict{Symbol, Set{Symbol}}()
    data = Dict{Symbol, Any}()
    for (id, val) in to_julia.params
        let_to_julia = setproperties(to_julia, variables=Set(), params=Dict())
        jval = let_to_julia(val)
        data[id] = jval
        deps[id] = let_to_julia.variables
    end
    extra, paramsorted = toposort(deps)
    params = Expr[]
    if !(:m in paramsorted)
        push!(paramsorted, :m)
        data[:m] = 1.0
    end
    for id in paramsorted
        if haskey(data, id)
            push!(params, Expr(:kw, id, data[id]))
        end
    end
    unpack_nodes = isempty(ckt_nodes) ? nothing :
        :(($(ckt_nodes...),) = $ParallelInstances(tuple, m)(nodes...))
    obs = [:($net_alias($net, $(QuoteNode(net)))) for net in ckt_nodes]
    lnn = name isa SNode ? LineNumberNode(name) : nothing
    mname = Symbol("@", fsym)
    push!(to_julia.exports, mname)
    return mname,
        @nolines :(@dyn function $fsym(🔍, nodes ; $(params...))
            $lnn
            (;$(paramsorted...)) = 🔍(;$(paramsorted...))
            $unpack_nodes
            $(obs...)
            @requires $(to_julia.variables...) $(extra...)
            $body
        end)
end

function (to_julia::SpcScope)(stmt::SNode{SP.MOSFET})
    ports = [stmt.d, stmt.g, stmt.s, stmt.b]
    spice_instance(to_julia, ports, stmt.name, stmt.model, stmt.parameters)
end

function (to_julia::SpcScope)(stmt::SNode{SP.BipolarTransistor})
    ports = [stmt.c, stmt.b, stmt.e]
    if stmt.s !== nothing
        push!(ports, stmt.s)
    end
    spice_instance(to_julia, ports, stmt.name, stmt.model, [])
end

function (to_julia::SpcScope)(stmt::SNode{SP.SubcktCall})
    subckt_instance(to_julia, stmt.nodes, stmt.name, stmt.model, stmt.parameters)
end

function (to_julia::SpcScope)(stmt::SNode{SP.Diode})
    spice_instance(to_julia, [stmt.pos, stmt.neg], stmt.name, stmt.model, stmt.params)
end

function hasparam(params, name)
    for p in params
        if LString(p.name) == name
            return true
        end
    end
    return false
end

function (to_julia::SpcScope)(stmt::SNode{SP.Resistor})
    # if params contains r or l, val is the model or nothing
    if hasparam(stmt.params, "l") || hasparam(stmt.params, "r")
        model = something(stmt.val, GlobalRef(SpectreEnvironment, :resistor))
        return spice_instance(to_julia, [stmt.pos, stmt.neg], stmt.name, model, stmt.params)
    else
        return spice_instance(to_julia, [stmt.pos, stmt.neg], stmt.name, GlobalRef(SpectreEnvironment, :resistor), stmt.params; r=stmt.val)
    end
end

function (to_julia::SpcScope)(stmt::SNode{SP.Inductor})
    spice_instance(to_julia, [stmt.pos, stmt.neg], stmt.name, GlobalRef(SpectreEnvironment, :inductor), stmt.params; l=stmt.val)
end

function (to_julia::SpcScope)(stmt::SNode{SP.Capacitor})
    spice_instance(to_julia, [stmt.pos, stmt.neg], stmt.name, GlobalRef(SpectreEnvironment, :capacitor), stmt.params; c=stmt.val)
end

function (to_julia::SpcScope)(stmt::SNode{<:Union{SP.Voltage, SP.Current}})
    constructor = if stmt isa SNode{SP.Voltage}
        GlobalRef(SpectreEnvironment, :vsource)
    elseif stmt isa SNode{SP.Current}
        GlobalRef(SpectreEnvironment, :isource)
    else
        error(@show stmt)
    end
    # TODO figure out the correct type for the current simulation
    kws = Expr[]
    for val in stmt.vals
        if val isa SNode{SP.DCSource}
            jval = to_julia(val.dcval)
            kw = Expr(:kw, :dc, jval)
            push!(kws, kw)
        elseif val isa SNode{SP.ACSource} # ACSource
            jval = to_julia(val.acmag)
            kw = Expr(:kw, :ac, jval)
            push!(kws, kw)
        elseif val isa SNode{SP.TranSource} # TranSource
            fname = LSymbol(val.kw)
            fn = getproperty(SpectreEnvironment, fname)
            if fname == :pwl
                # TODO: This isn't the correct module to use
                trancall = Expr(:call, fn, Expr(:macrocall, StaticArrays.var"@SVector", LineNumberNode(val),
                    Expr(:vect, (to_julia(v) for v in val.values)...)))
            elseif fname == :sin
                trancall = Expr(:call, SpectreEnvironment.spsin, (to_julia(v) for v in val.values)...)
            else
                trancall = Expr(:call, fn, (to_julia(v) for v in val.values)...)
            end
            kw = Expr(:kw, :tran, trancall)
            push!(kws, kw)
        else
            @show val
            error("unhandled voltage value $(String(val))")
        end
    end
    return spice_instance(to_julia, [stmt.pos, stmt.neg], stmt.name, constructor, kws)
end

function (to_julia::SpcScope)(stmt::SNode{SP.Behavioral})
    spice_instance(to_julia, [stmt.pos, stmt.neg], stmt.name, GlobalRef(SpectreEnvironment, :bsource), stmt.params)
end

function (to_julia::SpcScope)(stmt::SNode{SP.VCVS})
    if stmt.val === nothing
        return spice_instance(to_julia, [stmt.pos, stmt.neg], stmt.name, GlobalRef(SpectreEnvironment, :vcvs), stmt.params)
    else
        return spice_instance(to_julia, [stmt.pos, stmt.neg, stmt.cpos, stmt.cneg], stmt.name, GlobalRef(SpectreEnvironment, :vcvs), stmt.params; gain=stmt.val)
    end
end

function (to_julia::SpcScope)(stmt::SNode{SP.VCCS})
    if stmt.val === nothing
        spice_instance(to_julia, [stmt.pos, stmt.neg], stmt.name, GlobalRef(SpectreEnvironment, :vccs), stmt.params)
    else
        spice_instance(to_julia, [stmt.pos, stmt.neg, stmt.cpos, stmt.cneg], stmt.name, GlobalRef(SpectreEnvironment, :vccs), stmt.params; gain=stmt.val)
    end
end

function (to_julia::SpcScope)(stmt::SNode{SP.CCVS})
    spice_instance(to_julia, [stmt.pos, stmt.neg], stmt.name, GlobalRef(SpectreEnvironment, :ccvs), stmt.params; vnam=stmt.vnam, gain=stmt.val)
end

function (to_julia::SpcScope)(stmt::SNode{SP.CCCS})
    spice_instance(to_julia, [stmt.pos, stmt.neg], stmt.name, GlobalRef(SpectreEnvironment, :cccs), stmt.params; vnam=stmt.vnam, gain=stmt.val)
end

function (to_julia::SpcScope)(stmt::SNode{SP.Switch})
    on = LString(stmt.onoff) == "on"
    spice_instance(to_julia, [stmt.nd1, stmt.nd2, stmt.cnd1, stmt.cnd2], stmt.name, stmt.model, []; initial=on)
end

struct BinnedModel{B<:Tuple}
    scale::Float64
    bins::B
    BinnedModel(scale, bins::B) where B = new{B}(float(scale), bins)
end

const ParsedNT = NamedTuple{names, types} where {names, types<:Tuple{Vararg{Union{DefaultOr{Int}, DefaultOr{Float64}, DefaultOr{Bool}}}}}
struct ParsedModel{T}
    model::T
end
function ParsedModel(model, kwargs)
    ParsedModel{model}(model(;kwargs...))
end

Base.show(io::IO, m::ParsedModel) = print(io, "ParsedModel($(m.model), ...)")
Base.nameof(m::ParsedModel{T}) where T = nameof(T)
Base.nameof(m::BinnedModel) = nameof(first(m.bins))

modelfields(m) = ()
modelfields(m::DataType) = fieldnames(m)
modelfields(::Type{ParsedModel{T}}) where T = modelfields(T)
modelfields(::Type{BinnedModel{T}}) where T = modelfields(eltype(T))

Base.@assume_effects :foldable function case_adjust_kwargs_fallback(model::Type{T}, kwargs::NamedTuple{Names}) where {Names, T}
    case_insensitive = Dict(Symbol(lowercase(String(kw))) => kw for kw in fieldnames(T))
    pairs = Pair[]
    for kw in (Names::Tuple{Vararg{Symbol}})
        push!(pairs, get(case_insensitive, Symbol(lowercase(String(kw))), kw)=>getfield(kwargs, kw))
    end
    (; pairs...)
end

function _case_adjust_kwargs(model::Type{T}, kwargs::NamedTuple{Names}) where {Names, T}
    if @generated
        case_insensitive = Dict(Symbol(lowercase(String(kw))) => kw for kw in fieldnames(T))
        return :((;$(map(Names) do kw
            Expr(:kw,
                get(case_insensitive, Symbol(lowercase(String(kw))), kw),
                Expr(:call, :getfield, :kwargs, quot(Symbol(kw))))
        end...)))
    else
        return case_adjust_kwargs_fallback(model, kwargs)
    end
end

"""
    case_adjust_kwargs(model, kwargs)

Adjust the case of `kwargs` (which are assumed to be all lowercase) to match the
case of the fieldnames of `model`.
"""
Base.@assume_effects :total function case_adjust_kwargs(model::Type, kwargs::ParsedNT)
    #_uppercase_kwargs(model, kwargs)::NamedTuple{<:Any, types}
    _case_adjust_kwargs(model, kwargs)::ParsedNT
end

Base.@assume_effects :total function case_adjust_kwargs(model::Type, kwargs::NamedTuple)
    #_uppercase_kwargs(model, kwargs)::NamedTuple{<:Any, types}
    _case_adjust_kwargs(model, kwargs)::NamedTuple
end

function (pm::ParsedModel)(;kwargs...)
    setproperties(pm.model, values(kwargs))
end

struct NoBinExpection <: CedarException
    bm::BinnedModel
    l::Float64
    w::Float64
end
Base.showerror(io::IO, bin::NoBinExpection) = print(io, "NoBinExpection: no bin for BinnedModel $(typeof(bin.bm)) of size (l=$(bin.l), w=$(bin.w)).")

Base.@assume_effects :consistent :effect_free :terminates_globally @noinline function find_bin(bm::BinnedModel, l, w)
    l = bm.scale*l
    w = bm.scale*w
    for bin in bm.bins
        (; LMIN, LMAX, WMIN, WMAX) = bin.model
        if undefault(LMIN::DefaultOr{Float64}) <= l < undefault(LMAX::DefaultOr{Float64}) && undefault(WMIN::DefaultOr{Float64}) <= w < undefault(WMAX::DefaultOr{Float64})
            return bin
        end
    end
    throw(NoBinExpection(bm, l, w))
end

function (bm::BinnedModel)(; l, w, kwargs...)
    find_bin(bm, l, w)(; l, w, kwargs...)
end

"Instantiate a model using SPICE case insensitive semantics"
function spicecall(model; m=1.0, kwargs...)
    ParallelInstances(model(;kwargs...), m)
end

@Base.assume_effects :foldable function mknondefault_nt(nt::NamedTuple)
    if @generated
        names = Base._nt_names(nt)
        types = Any[]
        args = Any[]
        for i = 1:length(names)
            T = fieldtype(nt, i)
            arg = :(getfield(nt, $i))
            if T <: DefaultOr
                push!(args, arg)
            else
                push!(args, Expr(:new, DefaultOr{T}, arg, false))
                T = DefaultOr{T}
            end
            push!(types, T)
        end
        nttypes = Tuple{types...}
        Expr(:new, :(NamedTuple{$names, $nttypes}), args...)
    else
        map(mknondefault, nt)
    end
end

function spicecall(pm::ParsedModel{T}; m=1, kwargs...) where T
    instkwargs = case_adjust_kwargs(T, mknondefault_nt(values(kwargs)))::ParsedNT
    inst = setproperties(pm.model, instkwargs)
    ParallelInstances(inst, m)
end

function spicecall(bm::BinnedModel; l, w, kwargs...)
    spicecall(find_bin(bm, l, w); l, w, kwargs...)
end

spicecall(::Type{ParsedModel}, model, kwargs) = ParsedModel(model, case_adjust_kwargs(model, kwargs))

function make_binned_models_expr(to_julia::SpcScope, bins)
    maybescale = :(@isdefined(option_scale) ? option_scale : 1.0)
    map(collect(bins)) do (name, this_bins)
        push!(to_julia.exports, name)
        if to_julia.global_scope
            :(const $name = $(BinnedModel)($maybescale, ($(this_bins...),)))
        else
            quote
                @provides $name
                $name = $(BinnedModel)($maybescale, ($(this_bins...),))
            end
        end
    end
end

function (to_julia::SpcScope)(n::SNode{SP.Subckt})
    models = Any[]
    bins = Dict{Symbol, Vector{Symbol}}()
    instances = Any[]
    ckt_nodes = [Symbol(string("node_", n)) for n in LSymbol.(n.subckt_nodes)]
    nets = map(n->n=>:(), ckt_nodes)
    ckt_to_julia = setproperties(to_julia; nets=Dict(nets), params=Dict(), variables=Set([]), global_scope=false, is_circuit=true)
    for par in n.parameters
        id = LSymbol(par.name)
        ckt_to_julia.params[id] = par.val
        delete!(ckt_to_julia.variables, id)
    end
    # we don't support nested subcircuit definitions
    source_body(n, ckt_to_julia; models, instances, bins)
    binned_models = make_binned_models_expr(ckt_to_julia, bins)
    return subckt_macro(ckt_to_julia, n.name, ckt_nodes,
        @nolines quote
            $(models...)
            $(binned_models...)
            $(values(ckt_to_julia.nets)...)
            $(instances...)
        end)
end

function (to_julia::SpcScope)(n::SNode{SC.Subckt})
    models = Any[]
    bins = Dict{Symbol, Vector{Symbol}}()
    instances = Any[]
    ckt_nodes = [Symbol(string("node_", n)) for n in LSymbol.(n.subckt_nodes.nodes)]
    nets = map(n->n=>:(), ckt_nodes)
    ckt_to_julia = setproperties(to_julia; nets=Dict(nets), params=Dict(), variables=Set([]), global_scope=false, is_circuit=true)
    # we don't support nested subcircuit definitions
    source_body(n, ckt_to_julia; models, instances, bins)
    binned_models = make_binned_models_expr(ckt_to_julia, bins)
    return subckt_macro(ckt_to_julia, n.name, ckt_nodes,
        quote
            $(models...)
            $(binned_models...)
            $(values(ckt_to_julia.nets)...)
            $(instances...)
        end)
end

const JLPATH_PREFIX = "jlpkg://"

function resolve_julia_module(mod, components)
    submod = mod.get_module(joinpath(components[2:end]...))
    return submod
end

function resolve_module(path, section=nothing)
    @assert startswith(path, JLPATH_PREFIX)
    path = path[sizeof(JLPATH_PREFIX)+1:end]
    components = splitpath(path)
    @assert components[1] != "/"
    s1 = gensym()
    s2 = gensym()
    ret = quote
        import $(Symbol(components[1])) as $s1
        $s2 = $resolve_julia_module($s1, $(components))
    end
    if section === nothing
        push!(ret.args, :(using .$s2))
    else
        push!(ret.args, :(using .$s2.$section))
    end
    return ret
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
    cedarerror("include path $path not found in $includepaths or $pdkincludepaths")
end

function source_section(section::String, n::SNode{SpectreNetlistSource},
                        to_julia::SpcScope; kwargs...)
    for stmt in n.stmts
        if isa(stmt, SNode{SP.SPICENetlistSource})
            source_section(section, stmt, to_julia; kwargs...)
        else
            @show stmt
            error()
        end
    end
end

function source_section(section::String, n::SNode{SPICENetlistSource},
                        to_julia::SpcScope; kwargs...)
    for stmt in n.stmts
        if isa(stmt, SNode{SP.LibStatement})
            if LString(stmt.name) == LString(section)
                source_body(stmt, to_julia; kwargs...)
            end
        else
            @show stmt
            error()
        end
    end
end

function source_body(n::SNode{<:SC.AbstractBlockASTNode},
                     to_julia::SpcScope; instances::Vector{Any}, kwargs...)
    for stmt in n.stmts
        isa(stmt, SNode{SC.Simulator}) && continue
        if isa(stmt, SNode{SC.Instance})
            push!(instances, LineNumberNode(stmt))
            push!(instances, to_julia(stmt))
        elseif isa(stmt, SNode{SC.Parameters})
            to_julia(stmt)
        elseif isa(stmt, SNode{SC.Model})
            push!(kwargs[:models], LineNumberNode(stmt))
            push!(kwargs[:models], to_julia(stmt))
        elseif isa(stmt, SNode{SC.Global})
            # TODO revisit after subcircuits
        elseif isa(stmt, SNode{SC.AHDLInclude})
            str = strip(unescape_string(String(stmt.filename)), ['"', '\'']) # verify??
            ispdk, path = resolve_includepath(str, to_julia.includepaths, to_julia.pdkincludepaths)
            va = get!(() -> VerilogAParser.parsefile(path), to_julia.parsed_files, path)
            if va.ps.errored
                cedarthrow(LoadError(path, 0, VAParseError(va)))
            else
                vamod = va.stmts[end]
                name = Symbol(String(vamod.id))
                kwargs[:ckts][name] = make_module(va)
            end
        elseif isa(stmt, SNode{SC.Include})
            str = strip(unescape_string(String(stmt.filename)), '"') # verify??
            if startswith(str, JLPATH_PREFIX)
                section = stmt.section === nothing ? nothing : Symbol(String(stmt.section.section))
                mod = resolve_module(str, section)
                push!(kwargs[:includes], mod)
            else
                ispdk, path = resolve_includepath(str, to_julia.includepaths, to_julia.pdkincludepaths)
                sa = SpectreNetlistParser.parsefile(path; implicit_title=false);
                inc_to_julia = setproperties(to_julia,
                    includepaths=[dirname(path), to_julia.includepaths...],
                    is_circuit=to_julia.is_circuit && !ispdk)
                if stmt.section === nothing
                    source_body(sa, inc_to_julia; kwargs...)
                else
                    source_section(String(stmt.section.section), sa, inc_to_julia; kwargs...)
                end
            end
        elseif isa(stmt, SNode{SPICENetlistSource})
            source_body(stmt, to_julia; instances, kwargs...)
        elseif isa(stmt, SNode{SC.Subckt})
            name, ast = to_julia(stmt)
            kwargs[:ckts][name] = ast
        elseif isa(stmt, SNode{SC.Options})
            to_julia(stmt)
        else
            file = stmt.ps.srcfile.path
            line = SpectreNetlistParser.LineNumbers.compute_line(stmt.ps.srcfile.lineinfo, stmt.startof)
            @warn "Statement ignored: $(strip(String(stmt)))" _file=file _line=line
        end
    end
end

function extract_section_from_lib(p; section)
    for node in p.stmts
        if isa(node, SNode{SP.LibStatement})
            if lowercase(String(node.name)) == lowercase(section)
                return node
            end
        end
    end
    return nothing
end

const AnyInstance = Union{SNode{SP.MOSFET}, SNode{SP.SubcktCall},
    SNode{SP.Capacitor}, SNode{SP.Diode}, SNode{SP.BipolarTransistor},
    SNode{SP.Voltage}, SNode{SP.Current}, SNode{SP.Behavioral},
    SNode{SP.CCVS}, SNode{SP.CCCS}, SNode{SP.VCVS}, SNode{SP.VCCS},
    SNode{SP.Switch}, SNode{SP.Resistor}, SNode{SP.Inductor},
    SNode{SP.JuliaDevice}}

function cg_instance(to_julia::SpcScope, stmt::SNode)
    if isa(stmt, AnyInstance)
        return Expr(:block,
            LineNumberNode(stmt),
            to_julia(stmt))
    elseif isa(stmt, SNode{SP.IfBlock})
        head = :if
        local last_expr, expr
        for case in stmt.cases
            if_instances = Expr(:block)
            if case.condition === nothing
                this_expr = if_instances
            else
                this_expr = Expr(head, to_julia(case.condition.body), if_instances)
            end
            for stmt′ in case.stmts
                push!(if_instances.args, cg_instance(to_julia, stmt′))
            end
            @isdefined(last_expr) && push!(last_expr.args, this_expr)
            if !@isdefined(expr)
                expr = this_expr
            end
            last_expr = this_expr
            head = :elseif
        end
        return expr
    else
        cedarerror("Invalid statement in instance context")
    end
end

function source_body(n::SNode{<:SP.AbstractBlockASTNode}, to_julia::SpcScope; instances::Vector{Any}, kwargs...)
    for stmt in n.stmts
        if isa(stmt, SNode{SP.ParamStatement})
            to_julia(stmt)
        elseif isa(stmt, SNode{SP.Model})
            push!(kwargs[:models], LineNumberNode(stmt))
            push!(kwargs[:models], to_julia(stmt, kwargs[:bins]))
        elseif isa(stmt, SNode{SP.HDLStatement})
            str = strip(unescape_string(String(stmt.path)), ['"', '\'']) # verify??
            ispdk, path = resolve_includepath(str, to_julia.includepaths, to_julia.pdkincludepaths)
            va = get!(() -> VerilogAParser.parsefile(path), to_julia.parsed_files, path)
            if va.ps.errored
                cedarthrow(LoadError(path, 0, VAParseError(va)))
            else
                vamod = va.stmts[end]
                name = Symbol(String(vamod.id))
                macroname = Symbol("ckt_", lowercase(String(name)))
                modname = Symbol(name, "_module")
                kwargs[:ckts][name] = Expr(:toplevel,
                    make_module(va),
                    :(macro $macroname(🔍, nodes, args...)
                        dev = $modname.$name
                        esc(:(CedarSim.spicecall(CedarSim.ParsedModel($dev, (;)); $(args...))($nodes...)))
                    end))
            end
        elseif isa(stmt, SNode{SP.IncludeStatement})
            str = strip(unescape_string(String(stmt.path)), ['"', '\'']) # verify??
            if startswith(str, JLPATH_PREFIX)
                mod = resolve_module(str)
                push!(kwargs[:includes], mod)
            else
                ispdk, path = resolve_includepath(str, to_julia.includepaths, to_julia.pdkincludepaths)
                sa = get!(() -> SP.parsefile(path; implicit_title=false), to_julia.parsed_files, path)  # SPICE, Sprectre??
                inc_to_julia = setproperties(to_julia,
                    includepaths=[dirname(path), to_julia.includepaths...],
                    is_circuit=to_julia.is_circuit && !ispdk)
                source_body(sa, inc_to_julia; instances, kwargs...)
            end
        elseif isa(stmt, SNode{SP.LibInclude})
            str = strip(unescape_string(String(stmt.path)), ['"', '\'']) # verify??
            if startswith(str, JLPATH_PREFIX)
                mod = resolve_module(str, Symbol(String(stmt.name)))
                push!(kwargs[:includes], mod)
            else
                ispdk, path = resolve_includepath(str, to_julia.includepaths, to_julia.pdkincludepaths)
                section = String(stmt.name)
                p = get!(() -> SP.parsefile(path; implicit_title=false), to_julia.parsed_files, path)
                if (path, section) in to_julia.libraries
                    continue
                end
                push!(to_julia.libraries, (path, section))
                sa = extract_section_from_lib(p; section)
                if sa === nothing
                    cedarerror("Unable to find section named '$(section)'; malformed SPICE?")
                end
                inc_to_julia = setproperties(to_julia,
                    includepaths=[dirname(path), to_julia.includepaths...],
                    is_circuit=to_julia.is_circuit && !ispdk)
                source_body(sa, inc_to_julia; instances, kwargs...)
            end
        elseif isa(stmt, SNode{SP.LibStatement}) ||
               isa(stmt, SNode{SP.EndStatement})
            continue
        elseif isa(stmt, SNode{SP.Subckt})
            name, ast = to_julia(stmt)
            kwargs[:ckts][name] = ast
        elseif isa(stmt, AnyInstance) || isa(stmt, SNode{SP.IfBlock})
            push!(instances, cg_instance(to_julia, stmt))
        elseif isa(stmt, SNode{SP.OptionStatement}) ||
               isa(stmt, SNode{SP.TempStatement})
            to_julia(stmt)
        elseif isa(stmt, SNode{SP.Title}) ||
               isa(stmt, SNode{SP.Tran})
            # No codegen effect
        else
            file = stmt.ps.srcfile.path
            line = SpectreNetlistParser.LineNumbers.compute_line(stmt.ps.srcfile.lineinfo, stmt.startof)
            @warn "Statement ignored: $(strip(String(stmt)))" _file=file _line=line
        end
    end
end

function spec_params(to_julia::SpcScope)
    par = Dict{Symbol, Any}()
    for (id, val) in to_julia.params
        name = String(id)
        if startswith(name, "option_") && hasfield(SimSpec, Symbol(name[8:end]))
            par[Symbol(name[8:end])] = to_julia(val)
        end
    end
    if !isempty(par)
        s = gensym(:spec)
        kwargs = [Expr(:kw, k, :($isdefault($s.$k) ? $mknondefault(float($v)) : $s.$k)) for (k, v) in par]
        return :(let $s=$(spec)[]
            $setproperties($s; $(kwargs...))
        end)
    end
end

function sorted_sparams(global_to_julia, toplevel)
    deps = Dict{Symbol, Set{Symbol}}()
    data = Dict{Symbol, Any}()
    for (id, val) in global_to_julia.sparams
        let_to_julia = setproperties(global_to_julia, variables=Set(), params=Dict())
        jval = let_to_julia(val)
        data[id] = jval
        deps[id] = let_to_julia.variables
    end
    extra, paramsorted = toposort(deps)
    params = Expr[]
    for id in paramsorted
        if haskey(data, id)
            expr = if toplevel
                :(const $(id) = $(data[id]))
            else
                :($(id) = $(data[id]))
            end
            push!(params, expr)
        end
    end
    return params
end

"""
    make_spectre_netlist(n::SNode{<:Union{SpectreNetlistSource, SP.SPICENetlistSource}}, include_paths=[], toplevel=true)

Convert a Spectre netlist into a Julia expression.
Unlike make_spectre_circuit this function does not build a complete circuit.
Instead it is used to define parameters, models, and subcircuits.

If used at the top level (toplevel=true), parameters are const and evaluated at compile time,
else the generated code can be embedded in a larger circuit.
"""
function make_spectre_netlist(n::SNode{<:Union{SC.AbstractBlockASTNode, SP.AbstractBlockASTNode}}, include_paths=[], pdk_incldue_paths=[], toplevel=true)
    includes = Any[]
    models = Any[]
    ckts = Dict{Symbol, Expr}()
    bins = Dict{Symbol, Vector{Symbol}}()
    instances = Any[]
    global_to_julia = SpcScope(toplevel, false)
    append!(global_to_julia.includepaths, include_paths)
    append!(global_to_julia.pdkincludepaths, pdk_incldue_paths)
    source_body(n, global_to_julia; ckts, models, instances, bins, includes)
    binned_models = make_binned_models_expr(global_to_julia, bins)
    ckts = sort_ckts(ckts)
    if !isempty(global_to_julia.params)
        cedarthrow(ArgumentError("Circuit parameters outside circuit scope: $(keys(global_to_julia.params))"))
    end
    params = sorted_sparams(global_to_julia, toplevel)
    if !isempty(global_to_julia.parsed_files)
        @info "Includes:\n$(join(keys(global_to_julia.parsed_files), "\n"))"
    end
    exp = Expr(:export, global_to_julia.exports..., keys(global_to_julia.sparams)...)
    return @nolines Expr(toplevel ? :toplevel : :block,
        params...,
        models...,
        binned_models...,
        ckts...,
        values(global_to_julia.nets)...,
        instances...,
        exp)
end

function make_spectre_modules(n::SNode{<:Union{SC.AbstractBlockASTNode, SP.AbstractBlockASTNode}}; include_paths=[], pdk_include_paths=[], preload=[], exports=[], names=nothing)
    res = Expr(:toplevel)
    for node in n.stmts
        if isa(node, SNode{SP.LibStatement})
            if names !== nothing && String(node.name) ∉ names
                @info "Skipping $(String(node.name))"
                continue
            end
            expr = make_spectre_netlist(node, include_paths, pdk_include_paths)
            modname = Symbol(String(node.name))
            usings = []
            for pre in preload
                e = Expr(:using, Expr(:., Base.fullname(pre)...))
                push!(usings, e)
            end
            push!(res.args,
                :(module $modname
                    $(usings...)
                    $(expr.args...)
                    export $(exports...)
                end))
        else
            file = node.ps.srcfile.path
            line = SpectreNetlistParser.LineNumbers.compute_line(node.ps.srcfile.lineinfo, node.startof)
            @warn "Statement ignored: $(strip(String(node)))" _file=file _line=line
        end
    end
    return res
end

function load_spice_modules(file; kwargs...)
    sa = CedarSim.SpectreNetlistParser.SPICENetlistParser.SPICENetlistCSTParser.parsefile(file)
    make_spectre_modules(sa; kwargs...)
end

abstract type ParsedCircuit; end
function getsource end
getsource(circ::ParsedCircuit) = getsource(typeof(circ))

"""
    make_spectre_circuit(n::SNode{<:Union{SpectreNetlistSource, SP.SPICENetlistSource}}, include_paths=[]; name=gensym(:circuit))

Convert a Spectre netlist into a Julia expression.
Code generated by this function can be evaluated into a circuit that can be simulated.

The expression defines a macro for every (sub)circuit, and a wrapper function `name`.
The function takes an optional `ParamLens` to parameterize the circuit.
"""
function make_spectre_circuit(n::SNode{<:Union{SC.AbstractBlockASTNode, SP.AbstractBlockASTNode}}, include_paths=[], pdk_include_paths=[]; name=gensym(:circuit))
    includes = Any[]
    models = Any[]
    ckts = Dict{Symbol, Expr}()
    bins = Dict{Symbol, Vector{Symbol}}()
    instances = Any[]
    global_to_julia = SpcScope()
    append!(global_to_julia.includepaths, include_paths)
    append!(global_to_julia.pdkincludepaths, pdk_include_paths)
    source_body(n, global_to_julia; ckts, models, instances, bins, includes)
    binned_models = make_binned_models_expr(global_to_julia, bins)
    ckts = sort_ckts(ckts)
    params = sorted_sparams(global_to_julia, true)
    if !isempty(global_to_julia.parsed_files)
        @info "Includes:\n$(join(keys(global_to_julia.parsed_files), "\n"))"
    end
    # models are evaluated in the circuit scope so they see their parameters
    # circuits are evaluated in the global scope because they are macros
    sps = spec_params(global_to_julia)
    circtyp = gensym()
    return @nolines Expr(:toplevel,
    includes...,
    params...,
    ckts...,
    subckt_macro(global_to_julia, name, [],
    quote
        $(models...)
        $(binned_models...)
        $(values(global_to_julia.nets)...)
        $(instances...)
    end)[2],
    :(struct $circtyp <: $(ParsedCircuit) end),
    :($(GlobalRef(@__MODULE__, :getsource))(::Type{$circtyp}) = $(QuoteNode(n))),
    sps === nothing ?
    :(function (::$circtyp)(🔍::$AbstractParamLens=$ParamLens(NamedTuple()))
        $(subckt_instance(global_to_julia, [], nothing, name, []))
    end) :
    :(function (::$circtyp)(🔍::$AbstractParamLens=$ParamLens(NamedTuple()))
        @Base.with ($(spec) => $(sps)) $(subckt_instance(global_to_julia, [], nothing, name, []))
    end),
    :($(circtyp)()))
end

struct SpcFile
    file::String
    raw::Bool
end
SpcFile(file::String) = SpcFile(file, false)
Base.String(vaf::SpcFile) = vaf.file
Base.abspath(file::SpcFile) = SpcFile(Base.abspath(file.file))
Base.isfile(file::SpcFile) = Base.isfile(file.file)
Base.isabspath(file::SpcFile) = Base.isabspath(file.file)
Base.findfirst(str::SpcFile, file::SpcFile) = Base.findfirst(str, file.file)
Base.joinpath(str::SpcFile, file::SpcFile) = SpcFile(Base.joinpath(str, file.file))
Base.normpath(file::SpcFile) = SpcFile(Base.normpath(file.file))
export SpcFile

function Base.include(mod::Module, file::SpcFile)
    include_paths = [dirname(abspath(file.file)), pwd()]
    pdk, fname = resolve_includepath(file.file, include_paths)
    sa = SpectreNetlistParser.parsefile(fname; implicit_title=false)
    s = gensym()
    netlist = CedarSim.make_spectre_netlist(sa, include_paths)
    if file.raw
        Core.eval(mod, netlist)
    else
        Core.eval(mod, Expr(:toplevel, :(baremodule $s
                using ..CedarSim.SpectreEnvironment
                $(netlist)
            end), :(using .$s)))
    end
end

macro spc_str(str, flag="")
    include_paths = [dirname(String(__source__.file)), pwd()]
    enable_julia_escape = 'e' in flag
    inline = 'i' in flag
    sa = SpectreNetlistParser.parse(IOBuffer(str); enable_julia_escape)
    if sa.ps.errored
        cedarthrow(LoadError("sa_str", 0, ""))
    else
        # eval required to sequence macro definitions
        ast = CedarSim.make_spectre_netlist(sa, include_paths, [], !inline)
        :(Base.eval($__module__, $(QuoteNode(ast))))
    end
end

macro sp_str(str, flag="")
    include_paths = [dirname(String(__source__.file)), pwd()]
    enable_julia_escape = 'e' in flag
    inline = 'i' in flag
    sa = SpectreNetlistParser.parse(IOBuffer(str); start_lang=:spice, enable_julia_escape,
        implicit_title = !inline)
    if sa.ps.errored
        cedarthrow(LoadError("sa_str", 0, ""))
    elseif !inline
        return esc(CedarSim.make_spectre_circuit(sa, include_paths, []))
    else
        return esc(CedarSim.make_spectre_netlist(sa, include_paths, []))
    end
end

export @spc_str, @sp_str

function modify_spice(io::IO, node::SNode, nt::NamedTuple, startof)
    params = get(nt, :params, NamedTuple())
    for childnode in AbstractTrees.children(node)
        childnode === nothing && continue
        if (childnode isa SNode{<:SP.Parameter}
         && hasproperty(params, LSymbol(childnode.name))
         && getproperty(params, LSymbol(childnode.name)) isa Number
         && childnode.val !== nothing)
            val = getproperty(params, LSymbol(childnode.name))
            e = childnode.val
            endoff = e.startof+e.expr.off-1
            SpectreNetlistParser.RedTree.print_contents(io, node.ps, startof, endoff)
            print(io, val)
            startof = e.startof+e.expr.off+e.expr.width
        elseif isa(childnode, SNode{<:SP.Subckt}) ||
               isa(childnode, SNode{SP.Model}) ||
               isa(childnode, SNode{SP.SubcktCall}) ||
               isa(childnode, SNode{SP.MOSFET}) ||
               isa(childnode, SNode{SP.Capacitor}) ||
               isa(childnode, SNode{SP.Diode}) ||
               isa(childnode, SNode{SP.BipolarTransistor}) ||
               isa(childnode, SNode{SP.Voltage}) ||
               isa(childnode, SNode{SP.Current}) ||
               isa(childnode, SNode{SP.Resistor}) ||
               isa(childnode, SNode{SP.Inductor}) ||
               isa(childnode, SNode{SC.Model}) ||
               isa(childnode, SNode{SC.Instance})
            chnt = get(nt, LSymbol(childnode.name), NamedTuple())
            startof = modify_spice(io, childnode, chnt, startof)
        else
            startof = modify_spice(io, childnode, nt, startof)
        end
    end
    startof
end

function alter(io::IO, node::SNode, nt::NamedTuple)
    startof=node.startof+node.expr.off
    startof = modify_spice(io, node, canonicalize_params(nt), startof)
    endoff = node.startof+node.expr.off+node.expr.width-1
    SpectreNetlistParser.RedTree.print_contents(io, node.ps, startof, endoff)
end

"""
    alter([io], ast; kwargs...)
    alter([io], ast, nt::ParamSim)
    alter([io], ast, nt::ParamLens)

Print a netlist with the given parameters substituted.
Parameters in subcircuits can be passed as named tuples.
"""
alter(node::SNode; kwargs...) = alter(stdout, node, values(kwargs))
alter(node::SNode, nt::ParamSim) = alter(stdout, node, nt.params)
alter(node::SNode, nt::ParamLens) = alter(stdout, node, getfield(nt, :nt))
alter(io::IO, node::SNode; kwargs...) = alter(io, node, values(kwargs))
alter(io::IO, node::SNode, nt::ParamSim) = alter(io, node, nt.params)
alter(io::IO, node::SNode, nt::ParamLens) = alter(io, node, getfield(nt, :nt))


"""
    get_default_parameterization(ast)

Parse the AST for top-level `.param` statements with numeric values (purposefully
excluding `.param` statements that contain a computed expression as their value),
return them all (along with their values) for use as a default parameterization via
something like `ParamSim`.  Example usage:

    ast = SpectreNetlistParser.SPICENetlistParser.parsefile(path);
    code = CedarSim.make_spectre_circuit(sm.ast, [dirname(dirname(Base.pathof(CedarSim)))]);
    circuit_func = eval(code)
    ParamSim(circuit_func; get_default_parameterization(ast)...)
"""
function get_default_parameterization(ast; to_julia=SpcScope(), params = Pair[])
    for stmt in ast.stmts
        if isa(stmt, SNode{SPICENetlistSource})
            # Recurse into netlist source nodes
            get_default_parameterization(stmt; to_julia, params)
        elseif isa(stmt, SNode{SP.ParamStatement})
            for par in stmt.params
                if !isa(par.val, SNode{<:Union{SP.NumericValue, SC.NumericValue}})
                    continue
                end
                push!(params, LSymbol(par.name) => to_julia(par.val))
            end
        end
    end
    return params
end
