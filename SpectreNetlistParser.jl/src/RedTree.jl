module RedTree

using AbstractTrees
using ..EXPRS

import ..AbstractTerminal, ..isunit, ..istitle

export Node, NodeList, print_contents, fullcontents, reducedcontent

struct Node{T, PS}
    # TODO: Something lighter weight here?
    ps::PS
    startof::UInt32
    parent::Any
    expr::EXPR{T}
end

struct NodeList{PS, T}
    ps::PS
    startof::UInt32
    parent::Any
    expr::EXPRList{T}
end
AbstractTrees.parent(n::Union{Node, NodeList}) = n.parent
Base.length(nl::NodeList) = length(nl.expr)
Base.lastindex(nl::NodeList) = length(nl)
Node(ps, startof::UInt32, parent, l::EXPRList) =
    NodeList(ps, startof, parent, l)


function print_contents(io::IO, ps, file_pos_start::Integer, file_pos_end::Union{Integer, Nothing} = nothing)
    contents = ps.srcfile.contents
    if file_pos_end === nothing
        file_pos_end = (isa(contents, String) ? sizeof(contents) : length(contents.data))-1
    end
    range = (file_pos_start+1):(file_pos_end+1)
    if isa(contents, String)
        print(io, contents[range])
    else
        print(io, String(contents.data[range]))
    end
end

function Base.String(e::Node)
    buf = IOBuffer()
    print_vr = (e.startof+e.expr.off):(e.startof+e.expr.off+e.expr.width-1)
    print_contents(buf, e.ps, first(print_vr), last(print_vr))
    return String(take!(buf))
end
Base.Symbol(e::Node) = Symbol(String(e))


function fullcontents(n::Node)
    buf = IOBuffer()
    print_vr = n.startof:(n.startof+n.expr.fullwidth-1)
    print_contents(buf, n.ps, first(print_vr), last(print_vr))
    return String(take!(buf))
end

is_newline(node) = all(x->x∈('\r','\n'),String(node))
is_implicit_title(node) = istitle(node.expr.form) && node.dot === nothing

function reducedcontent(n::Node)
    buf = IOBuffer()
    pn = nothing
    for node in Leaves(n)
        node === nothing && continue
        # TODO: Hack for not having a UnitfulNumericalValue
        if !(isunit(node.expr.form) || is_implicit_title(AbstractTrees.parent(node)) || is_newline(node))
            print(buf, " ")
        end
        print(buf, String(node))
        pn = node
    end
    return String(take!(buf))
end

function Base.getproperty(e::Node, s::Symbol)
    hasfield(typeof(e), s) && return getfield(e, s)
    off = UInt32(0)
    for name in childnames(e.expr)
        ce = getproperty(e.expr, name)
        if name === s
            isa(ce, Union{EXPR, EXPRList}) || return ce
            return Node(e.ps, e.startof + off, e, ce)
        end
        (ce === nothing || !isa(ce, Union{EXPR, EXPRList})) && continue
        off += ce.fullwidth
    end
    error("No field $s")
end

function AbstractTrees.printnode(io::IO, e::Node{F}) where F
    print(io, F.name.name)
    print(io, " {w:", e.expr.width, " fw:", e.expr.fullwidth, " off:", e.expr.off, "} [", e.startof, ":", e.startof+e.expr.fullwidth-1, "]")
end

function AbstractTrees.printnode(io::IO, e::Node{F}) where F<:AbstractTerminal
    print(io, F.name.name)
    content = fullcontents(e)
    content = replace(content, ' ' => "␣")
    print(io, " {w:", e.expr.width, " fw:", e.expr.fullwidth, " off:", e.expr.off, "} [", e.startof, ":", e.startof+e.expr.fullwidth-1, "] \"", content, "\"")
end

function Base.show(io::IO, e::Node)
    print_tree(io, e)
end

struct NodeChildIterator{T<:Union{Node, NodeList}, S}
    node::T
    it::S
end
Base.length(nci::NodeChildIterator) = length(nci.it)
Base.keys(nci::NodeChildIterator) = Base.OneTo(length(nci))

function NodeChildIterator(e::Node)
    it = EXPRS.allchildren(e.expr)
    NodeChildIterator(e, it)
end


function NodeChildIterator(e::NodeList)
    it = e.expr.exprs
    NodeChildIterator(e, it)
end

function Base.iterate(it::NodeChildIterator)
    eit = iterate(it.it)
    eit === nothing && return nothing
    (e, st) = eit
    e === nothing && return (nothing, (it.node.startof, st))
    Node(it.node.ps, it.node.startof, it.node, e), (it.node.startof + e.fullwidth, st)
end

function Base.iterate(it::NodeChildIterator, (start, st))
    eit = iterate(it.it, st)
    eit === nothing && return nothing
    (e, st) = eit
    e === nothing && return (nothing, (start, st))
    Node(it.node.ps, start, it.node, e), (start + e.fullwidth, st)
end

AbstractTrees.children(e::Node{<:AbstractTerminal}) = ()
AbstractTrees.children(e::Node) = NodeChildIterator(e)

function Base.print(io::IO, e::NodeList)
    print(io, collect(NodeChildIterator(e)))
end

AbstractTrees.children(e::NodeList) = e
function Base.iterate(e::NodeList, it = (NodeChildIterator(e),))
    r = iterate(it...)
    r === nothing && return nothing
    r[1], (it[1], r[2])
end

function Base.getindex(e::NodeList, i::Int)
    # For convenience only. Do not use this in transformation code
    first(Iterators.drop(NodeChildIterator(e), i-1))
end

end # module
