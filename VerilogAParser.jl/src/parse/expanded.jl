using AbstractTrees: nodevalue

struct Node{T}
    # TODO: Something lighter weight here?
    ps::ParseState

    startof::VirtPos

    parent::Any

    expr::EXPR{T}
end
Base.lastindex(n::Node) = length(allchildren(n.expr))

struct NodeList{T}
    ps::ParseState

    startof::VirtPos

    parent::Any

    expr::EXPRList{T}
end
AbstractTrees.parent(n::Union{Node, NodeList}) = n.parent
AbstractTrees.ParentLinks(n::Union{Node, NodeList}) = AbstractTrees.StoredParents()

Base.length(nl::NodeList) = length(nl.expr)
Base.lastindex(nl::NodeList) = length(nl)
Node(ps::ParseState, startof::VirtPos, parent, l::EXPRList) =
    NodeList(ps, startof, parent, l)

spanvirtrange(n::Node) = (n.startof+n.expr.off):(n.startof+n.expr.off+n.expr.width-1)
virtrange(n::Node) = n.startof:(n.startof+n.expr.fullwidth-1)

function Base.String(e::Node)
    print_vr = spanvirtrange(e)
    tc = TreeCursor(ChunkTree(e.ps))
    leaf = nodevalue(findleafbyvirtrange(Leaves(tc), first(print_vr)))
    vr = virtrange(leaf)
    ivr = intersect(vr, print_vr)
    ivr == print_vr || error("Can not convert a non-terminal node to string")
    repr(leaf)[UInt32.(ivr .- (UInt32(first(vr)) - UInt32(1)))]
end
Base.Symbol(e::Node) = Symbol(String(e))

function fullcontents(n::Node)
    buf = IOBuffer()
    print_vr = virtrange(n)
    tc = AbstractTrees.TreeCursor(ChunkTree(n.ps))
    leaf = findleafbyvirtrange(Leaves(tc), first(print_vr))
    @assert leaf !== nothing
    for leaf in Iterators.flatten((
            (nodevalue(leaf),),
            Iterators.rest(Leaves(ChunkTree(n.ps)), AbstractTrees.next(AbstractTrees.LeavesState(tc)))))
        vr = virtrange(leaf)
        ivr = intersect(vr, print_vr)
        isempty(ivr) && break
        if ivr == vr
            show(buf, leaf)
        else
            buf2 = IOBuffer()
            show(buf2, leaf)
            prange = ivr .- UInt32(first(vr) - 1)
            prange = UInt32(first(prange)):UInt32(last(prange))
            write(buf, take!(buf2)[prange])
        end
    end
    return String(take!(buf))
end

function Base.propertynames(e::Node)
    fields = fieldnames(typeof(e))
    propnames = Base.propertynames(e.expr)
    @assert isdisjoint(fields,  propnames)
    (propnames..., fields...)
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
    error("type Node has no field $s")
end

function AbstractTrees.printnode(io::IO, e::Node{F}) where F
    print(io, F.name.name)
    print(io, " [", repr(UInt32(e.startof)), ":", repr(UInt32(e.startof+e.expr.fullwidth-1)), "]")
end

function AbstractTrees.printnode(io::IO, e::Node{F}) where F<:Terminal
    print(io, F.name.name)
    print(io, " [", repr(UInt32(e.startof)), ":", repr(UInt32(e.startof+e.expr.fullwidth-1)), "] \"", fullcontents(e), "\"")
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

function NodeChildIterator(e::Node{T}) where {T}
    it = allchildren(e.expr)
    NodeChildIterator(e, it)
end

function NodeChildIterator(e::NodeList)
    it = e.expr.exprs
    NodeChildIterator(e, it)
end

function Base.iterate(it::NodeChildIterator{T}) where {T}
    eit = iterate(it.it)
    eit === nothing && return nothing
    (e, st) = eit
    e === nothing && return (nothing, (it.node.startof, st))
    Node(it.node.ps, it.node.startof, it.node, e), (it.node.startof + e.fullwidth, st)
end

function Base.iterate(rit::Iterators.Reverse{NodeChildIterator{T}}) where {T}
    it = rit.itr
    eit = iterate(Reverse(it.it))
    eit === nothing && return nothing
    (e, st) = eit
    endat = it.node.startof + it.node.expr.fullwidth
    e === nothing && return (nothing, (endat, st))
    Node(it.node.ps, it.node.startof, it.node, e), (endat - e.fullwidth, st)
end

function Base.iterate(it::NodeChildIterator, (start, st))
    eit = iterate(it.it, st)
    eit === nothing && return nothing
    (e, st) = eit
    e === nothing && return (nothing, (start, st))
    Node(it.node.ps, start, it.node, e), (start + e.fullwidth, st)
end

function Base.iterate(rit::Iterators.Reverse{<:NodeChildIterator}, (endat, st))
    it = rit.itr
    @show (endat, it.it, st)
    eit = iterate(Iterators.Reverse(it.it), st)
    eit === nothing && return nothing
    (e, st) = eit
    e === nothing && return (nothing, (endat, st))
    Node(it.node.ps, start, it.node, e), (endat - e.fullwidth, st)
end

AbstractTrees.children(e::Node{<:Terminal}) = ()
AbstractTrees.children(e::Node) = NodeChildIterator(e)

function Base.print(io::IO, e::NodeList)
    print(io, collect(NodeChildIterator(e)))
end

function Base.iterate(e::NodeList, it = (NodeChildIterator(e),))
    r = iterate(it...)
    r === nothing && return nothing
    r[1], (it[1], r[2])
end

function Base.getindex(e::NodeList, i::Int)
    # For convenience only. Do not use this in transformation code
    first(Iterators.drop(NodeChildIterator(e), i-1))
end
