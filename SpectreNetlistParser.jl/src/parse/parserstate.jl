import ..SrcFile

mutable struct ParseState{L <: Lexer}
    srcfile::SrcFile
    lexer::L
    tok_storage::Token{Kind} # so we can look forward one token

    t::Token{Kind}
    nt::Token{Kind}
    startpos::UInt32
    prevpos::UInt32
    allpos::UInt32
    started::Bool
    errored::Bool
    lang_swapped::Bool
    line_offset::Int
end

function ParseState(io::IOBuffer; fname = nothing, enable_julia_escape::Bool=false, line_offset::Int=0)
    uz = UInt32(0)
    p = position(io)
    sstr = read(io, String)
    seek(io, p)
    sourcefile = SourceFile(sstr)

    ps = ParseState(SrcFile(fname, io, sourcefile), tokenize(io, ERROR, next_token; enable_julia_escape),
        Token(ERROR), Token(ERROR), Token(ERROR),
        UInt32(p), uz, UInt32(p),
        false,
        false,
        false,
        line_offset)
    get_next_token(ps)
    next(ps)
    return ps
end

function reinit_at_pos!(ps::ParseState, p)
    ps.allpos = p
    # TODO: Avoid creating a new lexer here?
    l = tokenize(ps.srcfile.contents, ERROR, next_token)
    ps.lexer = l
    ps.started=false
    ps.tok_storage = Token(ERROR)
    ps.t = Token(ERROR)
    ps.nt = Token(ERROR)
    get_next_token(ps)
    next(ps)
    return ps
end

function Base.show(io::IO, ps::ParseState)
    l = ps.lexer
    println(io, "ParseState at $(ps.srcfile.path) at $(position(l.io))")
    println(io, "nexts   : ", kind(ps.nt))
end

# nt(ps::ParseState, idx) = (idx == length(ps.nt) + 1) ? ps.ntlast : ps.nt[idx]
nt(ps::ParseState) = ps.nt

function get_next_token(ps)
    lex = ps.lexer
    tok = ps.tok_storage
    if kind(tok) != ENDMARKER
        toklen = tok.endbyte - tok.startbyte + 1
        ps.allpos += toklen
        ps.tok_storage = next_token(lex)
    end
    return tok
end

# This is kind of a mess...
function get_next_action_token(ps::ParseState, tok=get_next_token(ps))
    while kind(tok) == WHITESPACE || kind(tok) == COMMENT || kind(tok) == ESCD_NEWLINE || (kind(tok) == NEWLINE && !ps.started)
        tok = get_next_token(ps)
    end
    ps.started=true

    if kind(tok) == NEWLINE
        while kind(ps.tok_storage) == WHITESPACE
            get_next_token(ps)
        end
        if kind(ps.tok_storage) == Tokens.PLUS
            get_next_token(ps) # eat the plus
            return get_next_action_token(ps)
        end

        while true
            knt = kind(ps.tok_storage)
            if knt == WHITESPACE
                get_next_token(ps) # eat the whitespace
            elseif knt == NEWLINE || knt == COMMENT
                return get_next_action_token(ps)
            else
                return tok # significant newline..
            end
        end
    end

    return tok
end

function resolve_identifier(ps::ParseState, tok::Token)
    contents = ps.srcfile.contents
    if isa(contents, String)
        str = contents[(tok.startbyte:tok.endbyte) .+ 1]
    elseif isa(contents, IOBuffer)
        str = contents.data[(tok.startbyte:tok.endbyte) .+ 1]
    end
    return str
end


function next(ps::ParseState)
    ps.t = ps.nt
    ps.prevpos = ps.allpos
    ps.nt = get_next_action_token(ps)
    return ps
end

Base.position(ps::ParseState) = nt(ps).startbyte
