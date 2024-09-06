using .SpectreNetlistCSTParser: print_contents
using .SpectreNetlistCSTParser.RedTree: fullspan

function Base.show(io::IO, ::MIME"text/plain", @nospecialize(sp::SpCircuit{CktId})) where {CktId}
    sr = getsema(CktId)

    print(io, "Parsed ")

    if sr.kind === SPICECircuit
        print(io, "SPICE Circuit")
    elseif sr.kind === SpectreCircuit
        print(io, "Spectre Circuit")
    elseif sr.kind === SpectreCircuit
        print(io, "SPICE/Spectre Circuit")
    end

    if sr.title !== nothing
        print(io, " (")
        printstyled(io, String(sr.title.line), bold=true)
        print(io, ')')
    end
end

@enum RefKind begin
    Param
    Model
    Subckt
    Instance
    SPNet
    Ambiguous
end

struct MultipleKinds
    param::Union{Nothing, Pair{UInt64, SNode}}
    model::Union{Nothing, Pair{UInt64, SNode}}
    subckt::Union{Nothing, Pair{UInt64, SNode}}
    instance::Union{Nothing, Pair{UInt64, SNode}}
    net::Union{Nothing, Vector{Pair{UInt64, SNode}}}
end
function MultipleKinds(param::Union{Nothing, Vector}, model::Union{Nothing, Pair}, subckt::Union{Nothing, Pair}, instance::Union{Nothing, Pair}, net::Union{Nothing, Vector{Pair}})
    model !== nothing && (model = Pair{UInt64, SNode}(model))
    param !== nothing && (param = convert(Pair{UInt64, SNode}, param[end]))
    subckt !== nothing && (subckt = Pair{UInt64, SNode}(subck))
    instance !== nothing && (instance = convert(Pair{UInt64, SNode}, instance))
    net !== nothing && (net = Vector{Pair{UInt64, SNode}}(net))
    return MultipleKinds(param, model, subckt, instance, net)
end

struct SpRef
    ckt::SpCircuit
    path::Tuple{Symbol}
    kind::RefKind
    val::Union{Pair{UInt64, SNode}, Vector{Pair{UInt64, SNode}}, MultipleKinds}
end

Base.show(io::IO, ref::SpRef) = foreach(ref.path) do comp
        print(io, '.'); print(io, comp)
    end
function Base.show(io::IO, ::MIME"text/plain", ref::SpRef)
    if ref.kind == Ambiguous
        print(io, "Ambiguous")
    elseif ref.kind == Param
        print(io, "Parameter")
    elseif ref.kind == Model
        print(io, "Model")
    elseif ref.kind == Subckt
        print(io, "Subckt")
    elseif ref.kind == Instance
        print(io, "Instance")
    elseif ref.kind == SPNet
        print(io, "Net")
    end
    print(io, " ")
    print(io, "Reference ")
    path = sprint() do sio
        foreach(ref.path) do comp
            print(sio, '.'); print(sio, comp)
        end
    end
    printstyled(io, path, bold=true)
    println(io)
    sr = getsema(ref.ckt)
    if isa(ref.val, MultipleKinds)
    else
        lno = Base.LineNumberNode(ref.val[2])
        print(io, "╭ ")
        printstyled(something(lno.file, "Unknown File"), color=:light_black)
        println(io)
        printstyled(io, lno.line, color=:light_black)
        print(io, " ")
        instance = ref.val[2]
        printstyled(io, String(instance.name), bold=true)
        ns = sema_nets(instance)
        for net in ns
            nd = length(sr.nets[LSymbol(net)])
            @assert nd > 0
            print(io, " ")
            if nd == 1
                printstyled(io, String(net), color=:red)
            else
                printstyled(io, String(net), color=:green)
            end
        end
        print_contents(io, instance.ps, last(fullspan(last(ns)))+1, last(fullspan(instance)))
        print(io, "╰ ")
    end
end

function is_ambiguous(sr::SemaResult, sym::Symbol)
    paramref    = get(sr.params, sym, nothing)
    modelref    = get(sr.models, sym, nothing)
    subcktref   = get(sr.subckts, sym, nothing)
    instanceref = get(sr.instances, sym, nothing)

    return ((paramref !== nothing) + (modelref !== nothing) + (subcktref !== nothing) + (instanceref !== nothing)) > 1
end

function Base.getproperty(ckt::SpCircuit{CktId}, sym::Symbol) where {CktId}
    sr = getsema(CktId)
    sym = Symbol(lowercase(string(sym)))
    path = (sym,)

    paramref    = get(sr.params, sym, nothing)
    modelref    = get(sr.models, sym, nothing)
    subcktref   = get(sr.subckts, sym, nothing)
    instanceref = get(sr.instances, sym, nothing)
    netref      = get(sr.nets, sym, nothing)

    if ((paramref !== nothing) + (modelref !== nothing) + (subcktref !== nothing) + (instanceref !== nothing)) > 1
        return SpRef(ckt, path, Ambiguous, MultipleKinds(paramref, modelref, subcktref, instanceref, netref))
    elseif paramref !== nothing
        return SpRef(ckt, (Symbol(paramref[2].val.name),), Parameter, convert(Pair{UInt64, SNode}, paramref))
    elseif modelref !== nothing
        return SpRef(ckt, (Symbol(modelref[2].val.name),), Model, convert(Pair{UInt64, SNode}, modelref))
    elseif subcktref !== nothing
        return SpRef(ckt, (Symbol(subcktref[2].val.name),), Sbuckt, convert(Pair{UInt64, SNode}, subcktref))
    elseif instanceref !== nothing
        return SpRef(ckt, (Symbol(instanceref[2].val.name),), Instance, convert(Pair{UInt64, SNode}, instanceref[1]=>instanceref[2].val))
    else
        throw(FieldError(typeof(ckt), sym))
    end
end
