function Base.show(io::IO, circ::ParsedCircuit)
    S = getsource(circ)
    maybesp = first(children(S))
    if isa(maybesp, SNode{SP.SPICENetlistSource})
        print(io, "SPICE Circuit")
        for child in children(maybesp)
            if isa(child, SNode{SP.Title})
                print(io, " (")
                printstyled(io, String(child.line), bold=true)
                print(io, ')')
            end
        end
    else
        print(io, "Spectre Circuit")
    end
end

abstract type CircRef; end

# Various references to SPICE-namespaced things
struct InstanceRef
    ref::SNode
end
struct ParamRef
    ref::SNode
end
struct ModelRef
    ref::SNode
end
struct NetRef
    refs::Vector{SNode}
end

struct AmbiguousRef
    instance::Union{InstanceRef, Nothing}
    param::Union{ParamRef, Nothing}
    model::Union{ModelRef, Nothing}
    net::Union{NetRef, Nothing}
end

# Mapping of SPICE-namespce to DAECompiler namespace
function ScopeRef(sys::DAECompiler.IRODESystem, ref::NetRef)
    getproperty(sys, Symbol(string("node_", lowercase(String(ref.refs[1].name)))))
end


allnodes(child) = Iterators.filter(c->isa(c, SNode{SP.NodeName}), Iterators.drop(children(child), 1))
function Base.getproperty(circ::ParsedCircuit, sym::Symbol)
    S = getsource(circ)
    maybesp = first(children(S))
    param_candidate = instance_candidate = model_candidate = nothing
    nets = Vector{SNode}()
    if isa(maybesp, SNode{SP.SPICENetlistSource})
        S = maybesp
        for child in children(maybesp)
            if isa(child, SNode{SP.ParamStatement})
                for param in children(child.params)
                    if lowercase(String(sym)) == lowercase(String(param.name))
                        if param_candidate !== nothing
                            error("Duplicate parameter $sym")
                        end
                        param_candidate = ParamRef(param)
                    end
                end
            elseif isa(child, SNode{<:SP.AbstractInstanceNode})
                if lowercase(String(sym)) == lowercase(String(child.name))
                    if instance_candidate !== nothing
                        error("Duplicate instance $sym")
                    end
                    instance_candidate = InstanceRef(child)
                end
                for node in allnodes(child)
                    if lowercase(String(sym)) == lowercase(String(node))
                        push!(nets, node)
                    end
                end
            elseif isa(child, SNode{SP.Model})
                if lowercase(String(sym)) == lowercase(String(child.name))
                    if model_candidate !== nothing
                        error("Duplicate model $sym")
                    end
                    model_candidate = ModelRef(child)
                end
            end
        end
    else
        error("Not implemented for spectre")
    end
    nkinds = (param_candidate !== nothing) + (model_candidate !== nothing) + (instance_candidate !== nothing)
    if nkinds == 0 && isempty(nets)
        error("Circuit has no instance, model or net $sym")
    end
    if (nkinds == 1 && isempty(nets)) || nkinds == 0
        if param_candidate !== nothing
            return param_candidate
        elseif instance_candidate !== nothing
            return instance_candidate
        elseif model_candidate !== nothing
            return model_candidate
        else
            @assert !isempty(nets)
            return NetRef(nets)
        end
    end
    return AmbiguousRef(instance_candidate, param_candidate, model_candidate,
        isempty(nets) ? nothing : NetRef(nets))
end

function find_default_tspan(circ::ParsedCircuit)
    S = getsource(circ)
    maybesp = first(children(S))
    if isa(maybesp, SNode{SP.SPICENetlistSource})
        for child in children(maybesp)
            if isa(child, SNode{SP.Tran})
                tstart = 0.
                sc = SpcScope()
                if child.tstart !== nothing
                    tstart = sc(child.tstart)
                end
                tstop = sc(child.tstop)
                return (tstart, tstop)
            end
        end
    else
        error("Not implemented for Spectre circuit")
    end
    error("No .tran statement found in circuit. Add .tran or specify tspan manually.")
end

using SymbolicIndexingInterface
SymbolicIndexingInterface.symbolic_type(::NetRef) = ScalarSymbolic()
SymbolicIndexingInterface.symbolic_type(::Type{<:NetRef}) = ScalarSymbolic()
SymbolicIndexingInterface.is_independent_variable(sys::DAECompiler.TransformedIRODESystem,
    sym::NetRef) = false
function SymbolicIndexingInterface.is_variable(sys::DAECompiler.TransformedIRODESystem,
    sym::NetRef)
    SymbolicIndexingInterface.is_variable(sys, ScopeRef(DAECompiler.get_sys(sys), sym))
end
function SymbolicIndexingInterface.is_observed(sys::DAECompiler.TransformedIRODESystem,
    sym::NetRef)
    SymbolicIndexingInterface.is_observed(sys, ScopeRef(DAECompiler.get_sys(sys), sym))
end
SymbolicIndexingInterface.variable_index(sys::DAECompiler.TransformedIRODESystem,
    sym::NetRef) = SymbolicIndexingInterface.variable_index(sys, ScopeRef(DAECompiler.get_sys(sys), sym))
SymbolicIndexingInterface.is_parameter(sys::TransformedIRODESystem, sym::NetRef) = false
function (this::DAECompiler.DAEReconstructedObserved)(sym::NetRef, args...)
    this(ScopeRef(this.sys, sym), args...)
end
