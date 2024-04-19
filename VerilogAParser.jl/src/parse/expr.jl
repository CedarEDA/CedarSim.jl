using AbstractTrees

abstract type Terminal; end

struct EXPR{F}
    fullwidth::UInt32
    off::UInt32
    width::UInt32
    form::F
end
childnames(e::EXPR) = fieldnames(typeof(e.form))

mutable struct EXPRList{T} <: AbstractVector{EXPR{<:T}}
    fullwidth::Int
    exprs::Vector{EXPR{<:T}}
end
EXPRList{T}() where {T} = EXPRList{T}(0, Vector{EXPR{<:T}}())
Base.size(e::EXPRList) = size(e.exprs)
Base.length(e::EXPRList) = length(e.exprs)
Base.getindex(e::EXPRList, args...) =
    getindex(e.exprs, args...)
Base.iterate(e::EXPRList, args...) = Base.iterate(e.exprs, args...)
function Base.push!(e::EXPRList, arg::EXPR)
    e.fullwidth += arg.fullwidth
    push!(e.exprs, arg)
end

function Base.propertynames(e::EXPR)
    fields = fieldnames(typeof(e))
    propnames = Base.propertynames(e.form)
    @assert isdisjoint(fields,  propnames)
    (propnames..., fields...)
end

function Base.getproperty(e::EXPR, s::Symbol)
    hasfield(typeof(e), s) && return getfield(e, s)
    getproperty(getfield(e, :form), s)
end

function AbstractTrees.printnode(io::IO, e::EXPR{F}) where F
    print(io, F.name.name)
end

function Base.show(io::IO, e::EXPR)
    print_tree(io, e)
end

allchildren(e::EXPR) = allchildren(e.form)

AbstractTrees.children(e::EXPR{<:Terminal}) = ()
AbstractTrees.children(e::EXPR) = allchildren(e)
