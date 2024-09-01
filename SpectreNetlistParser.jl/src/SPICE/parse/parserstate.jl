import ...SrcFile

mutable struct ParseState{L <: Lexer}
    srcfile::SrcFile
    lexer::L
    tok_storage::Token{Kind} # so we can look forward one token

    t::Token{Kind}
    nt::Token{Kind}
    nnt::Token{Kind}
    # Corresponding to each token, the UInt32 of the start of the trivia for
    # that token, as well as the UInt32 of the (first byte of the) token itself
    startpos::UInt32
    pos::UInt32
    npos::UInt32
    nnpos::UInt32
    started::Bool
    errored::Bool
    lang_swapped::Bool
    return_on_language_change::Bool
    line_offset::Int
end

function ParseState(io::IOBuffer; fname::Union{AbstractString, Nothing} = nothing, return_on_language_change::Bool, enable_julia_escape::Bool=false, implicit_title::Bool=true, line_offset=0)
    p = position(io)
    sstr = read(io, String)
    seek(io, p)
    sourcefile = SourceFile(sstr)
    srcfile = SrcFile(fname, io, sourcefile)
    return ParseState(srcfile; return_on_language_change, enable_julia_escape, implicit_title, line_offset)
end


function ParseState(srcfile::SrcFile; return_on_language_change::Bool, enable_julia_escape::Bool=false, implicit_title::Bool=true, line_offset::Int=0)
    p = position(srcfile.contents)
    uz = UInt32(0)
    l = Lexer(srcfile.contents, ERROR, next_token; case_sensitive=false, enable_julia_escape)
    if implicit_title
        # First line of any SPICE file has an implicit .TITLE
        l.last_nontriv_token = TITLE
    end
    ps = ParseState(srcfile, l,
        Token(ERROR), Token(ERROR), Token(ERROR), Token(ERROR),
        UInt32(p), uz, uz, UInt32(p),
        false,
        false,
        false,
        return_on_language_change,
        line_offset)
    get_next_token(ps)
    next(ps)
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
nnt(ps::ParseState) = ps.nnt

function get_next_token(ps)
    lex = ps.lexer
    tok = ps.tok_storage
    if kind(tok) != ENDMARKER
        toklen = tok.endbyte - tok.startbyte + 1
        ps.nnpos += toklen
        if kind(tok) == JULIA_ESCAPE_BEGIN || kind(tok) == JULIA_ESCAPE
            # Synthesize body token
            ps.tok_storage = Token(JULIA_ESCAPE, Int(ps.nnpos), ps.nnpos-1)
        else
            ps.tok_storage = next_token(lex)
        end
    end
    return tok
end

# This is kind of a mess...
function get_next_action_token(ps::ParseState, tok=get_next_token(ps))
    while kind(tok) == WHITESPACE || kind(tok) == COMMENT || kind(tok) == ESCD_NEWLINE || (kind(tok) == NEWLINE && !ps.started)
        tok = get_next_token(ps)
    end
    ps.started = true

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
    ps.nt = ps.nnt
    ps.pos = ps.npos
    ps.npos = ps.nnpos
    ps.nnt = get_next_action_token(ps)
    return ps
end

Base.position(ps::ParseState) = nt(ps).startbyte
