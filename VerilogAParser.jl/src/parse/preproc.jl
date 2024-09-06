using Artifacts

const PARSER_LOOKAHEAD = 2
const PREPROC_LOOKAHEAD = 1

struct VirtPos
    pos::UInt32
end
Base.:+(a::VirtPos, b::Integer) = VirtPos(a.pos + UInt32(b))
Base.:-(a::VirtPos, b::Integer) = VirtPos(a.pos - UInt32(b))
Base.:-(a::VirtPos, b::VirtPos) = a.pos - b.pos
Base.isless(a::VirtPos, b::VirtPos) = isless(a.pos, b.pos)
Base.isfinite(a::VirtPos) = true
UInt32(pos::VirtPos) = pos.pos

# Scalar Behavior otherwise
Base.length(a::VirtPos) = 1
Base.iterate(a::VirtPos) = (a, nothing)
Base.iterate(a::VirtPos, b) = nothing

const VirtRange = typeof(VirtPos(0):VirtPos(1))

struct FormalArg
    name::Symbol
    namerange::VirtRange
    eq::Union{VirtPos, Nothing}
    defrange::Union{VirtRange, Nothing}
    default::Vector{Token{Kind}}
end

struct MacroDef
    name::Union{Symbol, Nothing}
    fidx::Int
    defrange::VirtRange
    tokens::Vector{Token{Kind}}
    formal_args::Vector{FormalArg}
    is_formal_arg::Bool
end
MacroDef(fidx::Int, tokens::Vector{Token{Kind}}) = MacroDef(nothing, fidx, VirtPos(0):VirtPos(0),
    tokens,
    Vector{FormalArg}(), true
)

struct MacroExpansion
    expansion_range::UnitRange{UInt32}
    def::MacroDef
    offsets::Vector{<:Any} # ExpansionInfo
end

struct ExpansionInfo
    # The last VirtPos that is still a part of this expansion.
    virt_pos_end::VirtPos
    file_pos_start::UInt32
    file_pos_end::UInt32
    # TODO: This representation is absolutely terrible.
    # We should be using a single immutaLble tree (e.g. an RRB tree
    # for this entire thing).
    which::Int
    macro_expansion::Union{Nothing, MacroExpansion}
end

struct MacroScope
    def::MacroDef
    actual_args::Vector{Vector{Token{Kind}}}
    offsets::Vector{ExpansionInfo}
    idx::Int
end
MacroScope(def::MacroDef, actual_args::Vector{Vector{Token{Kind}}}) =
    MacroScope(def, actual_args, Vector{ExpansionInfo}(), 1)
MacroScope(def::MacroDef) = MacroScope(def, Vector{Vector{Token{Kind}}}())

struct MacroErrorScope
    toks::Vector{Token{Kind}}
end

struct SrcFile
    path::Union{Nothing, String}
    contents::Union{String, IOBuffer}
    # Keeps a mapping of virtual byte position to file byte position at the
    # end of a macro expansion. Virtual position can be binary searched to
    # map arbitrary virual positions to file positions. In a properly incremental
    # parser, this would ideally be some sort of persistent datastructure like
    # an RRB tree, but for now this is dumb and simple.
    offsets::Vector{ExpansionInfo} # (virt_position, file_position, expansion_info)
    lineinfo::LineNumbers.SourceFile
end
SrcFile(path::Union{Nothing, String}, contents::Union{String, IOBuffer}) = SrcFile(path, contents, Vector{Pair{UInt32, UInt32}}(), SourceFile(contents))

mutable struct ParseState{L<:Lexer}
    srcfiles::Vector{SrcFile}
    lexer_stack::Vector{Tuple{Int, UInt32, L}}
    search_path::Vector{String}
    macros::Dict{Symbol, MacroDef}
    macrostack::Vector{Pair{Tuple{UInt32, UInt32}, MacroScope}}
    macro_arg_expansion_depth::Int
    macroerr::Union{Nothing, MacroErrorScope}
    ifdef_depth::Int
    done::Bool # Remove this
    lt::Token{Kind}
    t::Token{Kind}
    nt::NTuple{PARSER_LOOKAHEAD+PREPROC_LOOKAHEAD-1, Token{Kind}}
    ntlast::Token{Kind}
    # Corresponding to each token, the virtpos of the start of the trivia for
    # that token, as well as the virtpos of the (first byte of the) token itself
    virtpos::Tuple{VirtPos, VirtPos}
    nvirtpos::NTuple{PARSER_LOOKAHEAD+PREPROC_LOOKAHEAD-1, Tuple{VirtPos, VirtPos}}
    virtposlast::Tuple{VirtPos, VirtPos}
    allvirtpos::VirtPos
    errored::Bool
end
active_lexer(ps::ParseState) = ps.lexer_stack[end][3]

struct ChunkTree
    ps::ParseState
end
Base.length(tree::ChunkTree) = 1 + 2*length(tree.ps.srcfiles[1].offsets)

struct ChunkTreeNode
    ps::ParseState
    fidx::Int
    first_virt::VirtPos
    expansion_range::UnitRange{UInt32}
    offsets::Vector{ExpansionInfo}
    chunk_idx::Int
    expansion::Bool
end
AbstractTrees.SiblingLinks(::Union{Type{ChunkTreeNode}, Type{ChunkTree}}) = AbstractTrees.StoredSiblings()
AbstractTrees.ChildIndexing(::Union{ChunkTreeNode, ChunkTree}) = AbstractTrees.IndexedChildren()
AbstractTrees.childindices(c::Union{ChunkTreeNode, ChunkTree}) = 1:length(c)
AbstractTrees.children(c::Union{ChunkTreeNode, ChunkTree}) = c

struct TreeByOrdering <: Base.Ordering
    ctn::Union{ChunkTreeNode, ChunkTree}
end
Base.lt(treeorder::TreeByOrdering, a, i::Int) = isless(a, first(virtrange(nodevalue(treeorder.ctn[i]))))

function findleafbyvirtrange(leaves::Leaves, _virtrange)
    tree = leaves.root
    if isless(_virtrange, first(virtrange(nodevalue(tree))))
        return nothing
    end
    while true
        cs = children(tree)
        isempty(cs) && return tree
        chunk = nodevalue(cs)
        idx = searchsortedlast(1:length(chunk), _virtrange, TreeByOrdering(chunk))
        @assert idx !== 0
        tree = chunk[idx]
    end
end

function prevsibling(node::ChunkTreeNode)
    node.chunk_idx == 0 && error("Cannot compute previous node")
    ChunkTreeNode(node.ps, node.fidx,
        node.first_virt, node.expansion_range, node.offsets,
        node.expansion ? node.chunk_idx - 1 : node.chunk_idx,
        !node.expansion)
end

function virtrange(node::ChunkTree)
    srcfile = node.ps.srcfiles[1]
    srclen = isa(srcfile.contents, String) ? sizeof(srcfile.contents) : length(srcfile.contents.data)
    if length(srcfile.offsets) == 0
        VirtPos(0):VirtPos(srclen - 1)
    else
        last_chunk = srcfile.offsets[end]
        VirtPos(0):(last_chunk.virt_pos_end + (srclen - last_chunk.file_pos_end))
    end
end

function virtrange(node::ChunkTreeNode)
    if node.expansion
        return (last(virtrange(prevsibling(node)))+UInt32(1)):node.offsets[node.chunk_idx].virt_pos_end
    end

    if node.chunk_idx == 0
        virtstart = node.first_virt
        filestart = first(node.expansion_range)
    else
        ei = node.offsets[node.chunk_idx]
        virtstart = ei.virt_pos_end + 1
        filestart = ei.file_pos_end + 1
    end
    if node.chunk_idx == length(node.offsets)
        fileend = last(node.expansion_range)
    else
        next_ei = node.offsets[node.chunk_idx + 1]
        fileend = next_ei.file_pos_start-1
    end

    virtstart:(virtstart + fileend - filestart)
end

function filerange(tree::ChunkTreeNode)
    offsets = tree.offsets
    file_pos_end::UInt32 = first(tree.expansion_range)
    if tree.chunk_idx != 0
        ei = offsets[tree.chunk_idx]
        file_pos_end = ei.file_pos_end+1
    end
    if tree.expansion
        return ei.file_pos_start:ei.file_pos_end
    else
        if tree.chunk_idx == length(offsets)
            return file_pos_end:last(tree.expansion_range)
        else
            next_ei = offsets[tree.chunk_idx+1]
            return file_pos_end:(next_ei.file_pos_start-UInt32(1))
        end
    end
end

function file_range(ps::ParseState, fidx::Int)
    contents = ps.srcfiles[fidx].contents
    file_pos_end = (isa(contents, String) ? sizeof(contents) : length(contents.data))-1
    return UInt32(0):UInt32(file_pos_end)
end

is_macro_expansion(node::ChunkTreeNode) = node.expansion && node.offsets[node.chunk_idx].macro_expansion !== nothing
is_macro_expansion(node::ChunkTree) = false

function new_offsets(tree::ChunkTreeNode)
    offsets = tree.offsets
    ei = offsets[tree.chunk_idx]
    if ei.macro_expansion === nothing
        return (ei.which, tree.ps.srcfiles[ei.which].offsets, file_range(tree.ps, ei.which))
    else
        expansion = ei.macro_expansion
        return (ei.which, expansion.offsets, expansion.expansion_range)
    end
    return (ei.which, ei.macro_offsets === nothing ? tree.ps.srcfiles[ei.which].offsets : ei.macro_offsets)
end

Base.length(tree::ChunkTreeNode) = tree.expansion ? 1 + 2*length(new_offsets(tree)[2]) : 0

function print_contents(io::IO, ps::ParseState, fidx::Int, file_pos_start::UInt32, file_pos_end::Union{UInt32, Nothing} = nothing)
    contents = ps.srcfiles[fidx].contents
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

Base.String(tree::ChunkTreeNode) = repr(tree)
function Base.show(io::IO, tree::ChunkTreeNode)
    r = filerange(tree)
    print_contents(io, tree.ps, tree.fidx, first(r), last(r))
end

function Base.iterate(tree::ChunkTreeNode)
    tree.expansion || return nothing
    which, no, expansion_range = new_offsets(tree)
    node = ChunkTreeNode(tree.ps, which, last(virtrange(prevsibling(tree)))+1,
        expansion_range, no, 0, false)
    return node, node
end

function Base.getindex(tree::ChunkTreeNode, ind::Int)
    tree.expansion || return nothing
    idx, nisexpansion = divrem(ind, 2)
    which, no, expansion_range = new_offsets(tree)
    node = ChunkTreeNode(tree.ps, which, last(virtrange(prevsibling(tree)))+1,
        expansion_range, no, idx, nisexpansion == 0)
    return node
end

function Base.iterate(tree::ChunkTree)
    offsets = tree.ps.srcfiles[1].offsets
    node = ChunkTreeNode(tree.ps, 1, VirtPos(0), file_range(tree.ps, 1), offsets, 0, false)
    return node, node
end

function Base.getindex(tree::ChunkTree, ind::Int)
    idx, nisexpansion = divrem(ind, 2)
    offsets = tree.ps.srcfiles[1].offsets
    node = ChunkTreeNode(tree.ps, 1, VirtPos(0), file_range(tree.ps, 1),
        offsets, idx, nisexpansion == 0)
    return node
end

function AbstractTrees.nextsibling(node::ChunkTreeNode)
    new_node = ChunkTreeNode(node.ps, node.fidx, node.first_virt, node.expansion_range, node.offsets,
        node.chunk_idx + (node.expansion ? 0 : 1), !node.expansion)
    if new_node.chunk_idx > length(node.offsets)
        return nothing
    end
    return new_node
end

function AbstractTrees.prevsibling(node::ChunkTreeNode)
    node.chunk_idx == 0 && return nothing
    new_node = ChunkTreeNode(node.ps, node.fidx, node.first_virt, node.expansion_range, node.offsets,
        node.chunk_idx - (node.expansion ? 1 : 0), !node.expansion)
    return new_node
end

function Base.iterate(tree::Union{ChunkTree, ChunkTreeNode}, prev_node)
    new_node = AbstractTrees.nextsibling(prev_node)
    new_node === nothing && return nothing
    return new_node, new_node
end

# Provide an iterate method that uses indexing - makes AbstractTrees happy
Base.nextind(tree::Union{ChunkTree, ChunkTreeNode}, ind::Int) = ind + 1
function Base.iterate(tree::Union{ChunkTree, ChunkTreeNode}, ind::Int)
    1 <= ind <= length(tree) || return nothing
    tree[ind], ind+1
end

function ParseState(str::Union{IOBuffer,String}, fname::Union{String, Nothing} = nothing)
    uz = VirtPos(UInt32(0))
    macros = Dict{Symbol, MacroDef}()
    l = tokenize(str, ERROR, next_token)
    ps = ParseState{typeof(l)}(SrcFile[SrcFile(fname, str)], [(1,UInt32(0),l)], String[artifact"vams"], macros,
        Vector{MacroScope}(), 0, nothing, 0,
        false, Token(ERROR),
        Token(ERROR), ntuple(_->Token(ERROR), PARSER_LOOKAHEAD+PREPROC_LOOKAHEAD-1), Token(ERROR),
        (uz, uz), ntuple(_->(uz, uz), PARSER_LOOKAHEAD+PREPROC_LOOKAHEAD-1), (uz, uz), uz,
        false)
    fname !== nothing && push!(ps.search_path, dirname(fname))
    return next(next(next(ps)))
end

function ParseState(str::Union{IOBuffer,String}, loc::Int)
    ps = ParseState(str)
    prevpos = position(ps)
    while nt(ps).startbyte < loc
        next(ps)
        prevpos = loop_check(ps, prevpos)
    end
    return ps
end

function Base.show(io::IO, ps::ParseState)
    l = active_lexer(ps)
    println(io, "ParseState (depth $(length(ps.lexer_stack))) at $(position(l.io))")
    println(io, "last    : ", kind(ps.lt), " ($(ps.lt))")
    println(io, "current : ", kind(ps.t), " ($(ps.t))")
    println(io, "next    : ", kind(nt(ps)), " ($(nt(ps)))")
end
wstype(t::Token) = kind(t) == EmptyWS ? "empty" :
                    kind(t) == NewLineWS ? "ws w/ newline" :
                    kind(t) == SemiColonWS ? "ws w/ semicolon" : "ws"

function get_next_nontriv_token(ps, account = true)
    local tok
    while true
        tok = get_next_token(ps, account)
        kind(tok) != WHITESPACE && kind(tok) != COMMENT && break
    end
    return tok
end

function lex_arg_toks(ps, account=true)
    paren_depth = 0
    brace_depth = 0
    square_depth = 0
    top() = paren_depth == brace_depth == square_depth == 0
    local tok
    toks = Vector{Token{Kind}}()
    while true
        tok = get_next_token(ps, account)
        @case kind(tok) begin
            # XXX: This may mean this is better done in the lexer
            (LATTR | RATTR) => error("TODO")

            LPAREN => (paren_depth += 1)
            LSQUARE => (square_depth += 1)
            LBRACE => (brace_depth += 1)
            COMMA => top() && break
            # The standard says "matched pairs", so I'm gonna say these get
            # floored
            RSQUARE => (square_depth = max(0, square_depth - 1))
            RBRACE => (brace_depth = max(0, brace_depth - 1))
            RPAREN => begin
                top() && break
                paren_depth = max(0, paren_depth - 1)
            end
        end
        push!(toks, tok)
    end
    return (toks, tok)
end

function parse_macro_define!(ps, virtstart)
    id = get_next_nontriv_token(ps)

    if kind(id) == BACKTICK
        # Macro expansion inside macro definition
        error("TODO")
    end

    expansion_tokens = Vector{Token{Kind}}()
    formal_args = FormalArg[]

    tok = get_next_token(ps)
    if kind(tok) == BACKTICK
        error("TODO")
    elseif kind(tok) == LPAREN
        # Formal arguments
        while true
            argstartpos = ps.allvirtpos
            arg_name = get_next_nontriv_token(ps)
            sym = Symbol(untokenize(arg_name, active_lexer(ps)))
            toks, tok = lex_arg_toks(ps, true)
            argendpos = ps.allvirtpos - 2
            argstartpos = argendpos - (tok.endbyte - arg_name.startbyte - 1)
            push!(formal_args, FormalArg(sym, argstartpos:argendpos, nothing, nothing, toks))
            kind(tok) == RPAREN && break
            @assert kind(tok) == COMMA
        end
        tok = get_next_token(ps)
    end

    endvirtpos = ps.allvirtpos - 1

    while kind(tok) != NEWLINE && kind(tok) != ENDMARKER
        while kind(tok) == ESCD_NEWLINE
            # Skip token
            push!(expansion_tokens, Token(NEWLINE,
                tok.startbyte, tok.endbyte))
            tok = get_next_token(ps)
        end
        push!(expansion_tokens, tok)

        endvirtpos = ps.allvirtpos - 1
        tok = get_next_token(ps)
    end
    sym = Symbol(untokenize(id, active_lexer(ps)))
    return sym => MacroDef(sym, ps.lexer_stack[end][1], virtstart:endvirtpos, expansion_tokens, formal_args, false)
end

toklen(tok::Token) = tok.endbyte - tok.startbyte + 1

function get_next_token(ps, account = true)
    if ps.macroerr !== nothing
        tok = popfirst!(ps.macroerr.toks)
        if isempty(ps.macroerr.toks)
            ps.macroerr = nothing
        end
    elseif isempty(ps.macrostack)
        lex = active_lexer(ps)
        tok = next_token(lex)
    else
        scope = ps.macrostack[end][2]
        def = scope.def
        tokens = def.tokens
        if scope.idx > length(tokens)
            popped = pop!(ps.macrostack)
            virtpos = ps.allvirtpos-1
            offsets = isempty(ps.macrostack) ? ps.srcfiles[ps.lexer_stack[end][1]].offsets : ps.macrostack[end][2].offsets
            expanded_range = isempty(tokens) ? (UInt32(virtpos):UInt32(virtpos-1)) : tokens[1].startbyte:tokens[end].endbyte
            push!(offsets,
                ExpansionInfo(virtpos, popped[1]...,
                    popped[2].def.fidx,
                        MacroExpansion(
                            expanded_range,
                            popped[2].def,
                            popped[2].offsets)))
            ps.macro_arg_expansion_depth = length(ps.macrostack)
            if !isempty(ps.macrostack) && ps.macrostack[end][2].def.is_formal_arg
                matching = 0
                while matching > 0 || (ps.macro_arg_expansion_depth != 0 && ps.macrostack[ps.macro_arg_expansion_depth][2].def.is_formal_arg)
                    matching -= (ps.macrostack[ps.macro_arg_expansion_depth][2].def.is_formal_arg ? -1 : 1)
                    ps.macro_arg_expansion_depth -= 1
                end
            end
            return get_next_token(ps)
        end
        tok = def.tokens[scope.idx]
        ps.macrostack[end] = ps.macrostack[end][1] =>
            MacroScope(def, scope.actual_args, scope.offsets, scope.idx + 1)
    end
    if kind(tok) != ENDMARKER && account
        toklen = tok.endbyte - tok.startbyte + 1
        ps.allvirtpos += toklen
    end
    return tok
end

function preproc_skip!(ps)
    nest = 0
    local tok
    while true
        tok = get_next_token(ps)
        kind(tok) == ENDMARKER && error(ps, UnexpectedToken)
        kind(tok) != BACKTICK && continue
        tok = get_next_token(ps)
        @case kind(tok) begin
            (M_IFDEF | M_IFNDEF) => (nest += 1)
            (M_ELSE | M_ELSIF | M_ENDIF) => begin
                if nest == 0
                    break
                end
                kind(tok) == M_ENDIF && (nest -= 1)
            end
            _ => continue
        end
    end
    return tok
end

function shift_tuple(tup, last)
    return first(tup), tuple(Base.tail(tup)..., last)
end

nt(ps::ParseState, idx) = (idx == length(ps.nt) + 1) ? ps.ntlast : ps.nt[idx]
nt(ps::ParseState) = nt(ps, 1)

function get_next_action_token(ps::ParseState, startvirtpos = nothing, tok=get_next_token(ps))
    if startvirtpos === nothing && tok.startbyte == 0
        startvirtpos = VirtPos(0)
    end

    # Skip any whitespace
    while kind(tok) == WHITESPACE || kind(tok) == COMMENT || kind(tok) == NEWLINE
        if kind(tok) == NEWLINE && startvirtpos === nothing
            startvirtpos = ps.allvirtpos
        end
        tok = get_next_token(ps)
    end
    toklen = kind(tok) == ENDMARKER ? 0 : (tok.endbyte - tok.startbyte + 1)
    (tok, (something(startvirtpos, ps.allvirtpos - toklen), ps.allvirtpos-toklen))
end

function resolve_identifier(ps::ParseState, fidx::Int, tok::Token)
    contents = ps.srcfiles[fidx].contents
    if isa(contents, String)
        str = contents[(tok.startbyte:tok.endbyte) .+ 1]
    elseif isa(contents, IOBuffer)
        str = String(contents.data[(tok.startbyte:tok.endbyte) .+ 1])
    end
    if kind(tok) == ESCD_IDENTIFIER
        str = str[2:end]
    end
    return str
end

function active_file(ps)
    if isempty(ps.macrostack)
        return ps.lexer_stack[end][1]
    else
        return ps.macrostack[end][2].def.fidx
    end
end

global debug = false

function maybe_macroexpand_formal_arg!(ps::ParseState, tok)
    # The spec allows formal argument substitution wherever an identifier is
    # valid, so try to do that now.
    if kind(tok) == IDENTIFIER && ps.macro_arg_expansion_depth > 0 && length(ps.macrostack[ps.macro_arg_expansion_depth][2].def.formal_args) != 0
        sym = Symbol(resolve_identifier(ps, active_file(ps), tok))
        for (i, (;name,)) in enumerate(ps.macrostack[ps.macro_arg_expansion_depth][2].def.formal_args)
            if name == sym
                fidx = ps.macro_arg_expansion_depth > 1 ? ps.macrostack[ps.macro_arg_expansion_depth-1][2].def.fidx :
                    ps.lexer_stack[end][1]

                # Exclude formal argument expansion from the virtrange
                ps.allvirtpos -= tok.endbyte - tok.startbyte + 1

                # Just treat this the same as a macroexpansion for now
                push!(ps.macrostack, (tok.startbyte, tok.endbyte)=>MacroScope(MacroDef(
                    fidx,
                    ps.macrostack[ps.macro_arg_expansion_depth][2].actual_args[i]
                )))

                matching = 1
                while matching > 0 || (ps.macro_arg_expansion_depth != 0 && ps.macrostack[ps.macro_arg_expansion_depth][2].def.is_formal_arg)
                    matching -= (ps.macrostack[ps.macro_arg_expansion_depth][2].def.is_formal_arg ? -1 : 1)
                    ps.macro_arg_expansion_depth -= 1
                end

                return true
            end
        end
    end
    return false
end

function next(ps::ParseState)
    #  shift old tokens
    ps.lt = ps.t
    ps.t, ps.nt = shift_tuple(ps.nt, ps.ntlast)
    ps.virtpos, ps.nvirtpos = shift_tuple(ps.nvirtpos, ps.virtposlast)
    @goto next_token

@label next_two_tokens
    tok1, virtpos1 = get_next_action_token(ps, ps.nvirtpos[2][1])
    ps.nt = (ps.nt[1], tok1)
    ps.nvirtpos = (ps.nvirtpos[1], virtpos1)

@label got_2
    if kind(ps.nt[2]) == ENDMARKER && length(ps.lexer_stack) != 1
        popped = pop!(ps.lexer_stack)
        fidx = ps.lexer_stack[end][1]
        virtposend = ps.allvirtpos - 1
        tok1 = get_next_token(ps)
        npos = tok1.startbyte - 1
        tok1, virtpos1 = get_next_action_token(ps, virtpos1[1], tok1)
        ps.nt = (ps.nt[1], tok1)
        ps.nvirtpos = (ps.nvirtpos[1], virtpos1)
        push!(ps.srcfiles[fidx].offsets,
            ExpansionInfo(
                virtposend, popped[2], npos, popped[1], nothing))
        @goto got_2
    end

    # Check if this needs to be macroexpanded
    maybe_macroexpand_formal_arg!(ps, tok1) && @goto next_two_tokens

@label next_token
    ps.ntlast, ps.virtposlast = get_next_action_token(ps)
    is_at_start = false

@label got_token
    # Pop lexers if we've reached the end of the file
    if kind(ps.ntlast) == ENDMARKER && length(ps.lexer_stack) != 1
        popped = pop!(ps.lexer_stack)
        fidx = ps.lexer_stack[end][1]
        virtposend = ps.allvirtpos - 1
        ps.ntlast = get_next_token(ps)
        npos = ps.ntlast.startbyte - 1
        ps.ntlast, ps.virtposlast = get_next_action_token(ps, ps.virtposlast[1])
        push!(ps.srcfiles[fidx].offsets,
            ExpansionInfo(
                virtposend, popped[2], npos, popped[1], nothing))
        @goto got_token
    end

    # Macro expansion needs one look-ahead token for error recovery, so
    # operate on nnt here, which is the farthest lookahead that the parser
    # proper does.
    nt2 = nt(ps, 2)

    if kind(nt2) == BACKTICK
        # Some kind of compiler directive
        cnt = nt(ps, 3)

        if kind(cnt) == M_DEFINE
            def = parse_macro_define!(ps, ps.nvirtpos[2][2])
            ps.macros[def[1]] = def[2]
            @goto next_two_tokens

        elseif kind(cnt) == M_INCLUDE
            # Exclude macro expansion from the virtrange
            ps.allvirtpos -= cnt.endbyte - nt2.startbyte + 1

            # TODO: Forbid semantic tokens before this on the include line
            triv1 = get_next_token(ps, false)
            isquote = kind(triv1) == M_INCLUDE_TRIVIA_ANGLE
            fname = get_next_token(ps, false)
            triv2 = get_next_token(ps, false)

            fname = untokenize(fname, active_lexer(ps))
            if !isabspath(fname)
                for path in ps.search_path
                    jpath = joinpath(path, fname)
                    if isfile(jpath)
                        fname = jpath
                        break
                    end
                end
            end

            buf = IOBuffer(open(read, fname))
            push!(ps.srcfiles, SrcFile(fname, buf))
            push!(ps.lexer_stack, (length(ps.srcfiles), nt2.startbyte, tokenize(buf, ERROR, next_token)))
            @goto next_two_tokens

        elseif kind(cnt) == M_UNDEF
            id = get_next_nontriv_token(ps)
            sym = Symbol(untokenize(id, active_lexer(ps)))
            delete!(ps.macros, sym)

        elseif kind(cnt) in (M_IFDEF, M_IFNDEF)
            id = get_next_nontriv_token(ps)
            if !isident(kind(id))
                error(ps, UnexpectedToken)
            end

            while true
                sym = Symbol(resolve_identifier(ps, active_file(ps), id))

                defined = haskey(ps.macros, sym)
                kind(cnt) == M_IFNDEF && (defined = !defined)

                if defined
                    ps.ifdef_depth += 1
                    break
                end

                cnt = preproc_skip!(ps)
                kind(cnt) == M_ENDIF && break

                if kind(cnt) == M_ELSE
                    ps.ifdef_depth += 1
                    break
                end

                @assert kind(cnt) == M_ELSIF
                id = get_next_nontriv_token(ps)
            end
            @goto next_two_tokens

        elseif kind(cnt) in (M_ELSIF, M_ELSE, M_ENDIF)
            if ps.ifdef_depth == 0
                error(ps, UnexpectedToken)
            end
            ps.ifdef_depth -= 1
            if kind(cnt) != M_ENDIF
                while kind(preproc_skip!(ps)) != M_ENDIF
                end
            end
            @goto next_two_tokens
        end

        if !isident(kind(cnt)) && !iskeyword(kind(cnt))
            synthesize_token!(ps, nt2, 2, PREPROC_ERR_MISSING_MACRO)
            @goto done_macro
        end

        macroname = Symbol(resolve_identifier(ps, active_file(ps), cnt))

        function synthensize_macro_token!(kind)
            ntuple(length(ps.nt)) do i
                i != 2 && return ps.nt[i]
                tok = ps.nt[i]
                return Token(kind, nt2.startbyte,
                    cnt.endbyte)
            end
        end

        if !haskey(ps.macros, macroname)
            ps.nt = synthensize_macro_token!(PREPROC_ERR_UNDEF_MACRO)
            @goto next_token
        end

        def = ps.macros[macroname]

        if !isempty(ps.macrostack) && ps.macrostack[end][2].def == def
            ps.nt = synthensize_macro_token!(PREPROC_ERR_RECURSIVE_MACRO)
            @goto next_token
        end

        expansion_end = cnt.endbyte

        # The parsing here depends on whether the macro has formal args or not
        actual_args = Vector{Token{Kind}}[]
        if !isempty(def.formal_args)
            lp = get_next_nontriv_token(ps, false)
            if kind(lp) != LPAREN
                ps.nt = synthensize_macro_token!(PREPROC_ERR_EXPECTED_LPAREN)
                @goto next_token
            end

            commas = Token{Kind}[]

            cur_arg = 1
            while true
                toks, tok = lex_arg_toks(ps, false)
                if all(x->kind(x) == WHITESPACE, toks)
                    toks = Token{Kind}[]
                end
                push!(actual_args, toks)
                push!(commas, tok)
                expansion_end = tok.endbyte
                kind(tok) == RPAREN && break
                cur_arg += 1
            end

            if length(actual_args) != length(def.formal_args)
                ps.nt = synthensize_macro_token!(PREPROC_ERR_BAD_MACRO_CALL)
                error_tokens = [lp]
                for i in 1:length(actual_args)
                    arg_tokens = actual_args[i]
                    comma = commas[i]
                    if length(arg_tokens) == 0
                        prev_tok = i == 1 ? lp : commas[i-1]
                        push!(error_tokens, Token(PREPROC_ERR_BAD_MACRO_PARAM,
                            prev_tok.endbyte+1,
                            comma.startbyte-1))
                    else
                        push!(error_tokens, Token(PREPROC_ERR_BAD_MACRO_PARAM,
                            arg_tokens[1].startbyte,
                            arg_tokens[end].endbyte))
                    end
                    push!(error_tokens, comma)
                end
                ps.macroerr = MacroErrorScope(error_tokens)
                @goto next_token
            end

            # Fill in default expansion
            for i = 1:length(def.formal_args)
                if isempty(actual_args[i])
                    actual_args[i] = def.formal_args[cur_arg].default
                end
            end
        end

        # Exclude macro expansion from the virtrange.
        # N.B.: All tokens after `cnt` were retrieved with accounting off, so
        # we do not need to adjust for them here.
        ps.allvirtpos -= cnt.endbyte - nt2.startbyte + 1
        push!(ps.macrostack, (nt2.startbyte, expansion_end)=>MacroScope(def, actual_args))
        ps.macro_arg_expansion_depth = length(ps.macrostack)

        @goto next_two_tokens
    end

@label done_macro

    # The spec allows formal argument substitution wherever an identifier is
    # valid, so try to do that now.
    maybe_macroexpand_formal_arg!(ps, ps.ntlast) && @goto next_token

    return ps
end

function synthesize_token!(ps, tokstart, idx, err)
    ps.nt = ntuple(length(ps.nt)) do i
        i != idx && return ps.nt[i]
        tok = ps.nt[i]
        return Token(err, tokstart.startbyte, tok.endbyte)
    end
end

Base.position(ps::ParseState) = nt(ps).startbyte
