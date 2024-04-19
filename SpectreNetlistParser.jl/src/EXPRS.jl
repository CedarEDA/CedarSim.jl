module EXPRS

using AbstractTrees

export EXPR, EXPR!, EXPRList, allchildren, childnames

abstract type AbstractTerminal end

struct EXPR{F}
    fullwidth::UInt32
    off::UInt32
    width::UInt32
    form::F
end

function EXPR(val)
    firstchild = nothing
    lastchild = nothing
    fw::UInt32 = 0
    for child in EXPRS.allchildren(val)
        child === nothing && continue
        if firstchild === nothing
            firstchild = child
        end
        lastchild = child
        fw += child.fullwidth
    end
    if firstchild isa EXPRList
        firstchild = firstchild.exprs[1]
    end
    if lastchild isa EXPRList
        lastchild = lastchild.exprs[end]
    end
    # Write this in a nicer way?
    firstchild_off = firstchild === nothing ? UInt32(0) : firstchild.off
    lastchild_off = lastchild === nothing ? UInt32(0) : lastchild.off
    lastchild_width = lastchild === nothing ? UInt32(0) : lastchild.width
    lastchild_fullwidth = lastchild === nothing ? UInt32(0) : lastchild.fullwidth

    EXPR(fw, firstchild_off, fw - firstchild_off - (lastchild_fullwidth - (lastchild_width+lastchild_off)), val)
end


childnames(e::EXPR) = fieldnames(typeof(e.form))

mutable struct EXPRList{T} <: AbstractVector{EXPR{<:T}}
    fullwidth::UInt32
    exprs::Vector{EXPR{<:T}}
end
EXPRList{T}() where {T} = EXPRList{T}(0, Vector{EXPR{<:T}}())
Base.size(e::EXPRList) = size(e.exprs)
Base.getindex(e::EXPRList, args...) = getindex(e.exprs, args...)
Base.iterate(e::EXPRList, args...) = iterate(e.exprs, args...)
allchildren(e::EXPRList) = e
function Base.push!(e::EXPRList, arg)
    e.fullwidth += arg.fullwidth
    push!(e.exprs, arg)
end
function Base.pop!(e::EXPRList)
    v = pop!(e.exprs)
    e.fullwidth -= v.fullwidth
    return v
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

AbstractTrees.children(e::EXPR) = allchildren(e)

end
