const EOF_CHAR = typemax(Char)

eof(io::IO) = Base.eof(io)
eof(c::Char) = c === EOF_CHAR

readchar(io::IO) = eof(io) ? EOF_CHAR : read(io, Char)

mutable struct Lexer{IO_t <: IO, K, F}
    io::IO_t
    io_startpos::Int

    next_token::F
    case_sensitive::Bool

    token_startpos::Int

    current_pos::Int

    last_token::K
    last_nontriv_token::K
    charstore::IOBuffer
    chars::Tuple{Char,Char,Char}
    charspos::Tuple{Int,Int,Int}
    doread::Bool

    # SPICE specific, move to a spice specific lexer on top of this?
    lexed_nontriv_token_line::Bool
    # non-empty if we are currently in an expression
    # node names can contain characters that are considered operators in expressions
    # expressions are started by ', {, =, and some dot commands
    # and ended by respectively  ', }, whitespace, and newlines
    lexing_expression_stack::Vector{K}
    spice_dialect::Symbol

    # In strict mode, the parser will reject extensions that are not part of the
    # correct spice_dialect, even if they are unambiguous
    strict::Bool

    # Whether we allow escaping to Julia
    enable_julia_escape::Bool
end

function Lexer(io::IO_t, init_kind::K, next_token; case_sensitive::Bool=true, enable_julia_escape::Bool=false) where {IO_t, K}
    c1 = ' '
    p1 = position(io)
    if eof(io)
        c2, p2 = EOF_CHAR, p1
        c3, p3 = EOF_CHAR, p1
    else
        c2 = read(io, Char)
        case_sensitive || (c2 = uppercase(c2))
        p2 = position(io)
        if eof(io)
            c3, p3 = EOF_CHAR, p1
        else
            c3 = read(io, Char)
            case_sensitive || (c3 = uppercase(c3))
            p3 = position(io)
        end
    end
    l = Lexer(io, position(io), next_token, case_sensitive, position(io), position(io), init_kind, init_kind, IOBuffer(), (c1,c2,c3), (p1,p2,p3), false, false, K[], :ngspice, false, enable_julia_escape)
    start_token!(l)
    return l
end
Lexer(str::AbstractString, init_kind, next_token; case_sensitive=true, enable_julia_escape::Bool=false)  = Lexer(IOBuffer(str), init_kind, next_token; case_sensitive)

"""
    tokenize(x, T = Token)

Returns an `Iterable` containing the tokenized input. Can be reverted by e.g.
`join(untokenize.(tokenize(x)))`. Setting `T` chooses the type of token
produced by the lexer (`Token` or `Token`).
"""
tokenize(x, init_kind, next_token; case_sensitive=true, enable_julia_escape::Bool=false) = Lexer(x, init_kind, next_token; case_sensitive, enable_julia_escape)

function untokenize(t::Token, l::Lexer{IOBuffer})
    String(l.io.data[1 .+ (t.startbyte:t.endbyte)])
end

# Iterator interface
Base.IteratorSize(::Type{<:Lexer}) = Base.SizeUnknown()
Base.IteratorEltype(::Type{<:Lexer})  = Base.HasEltype()
Base.eltype(::Type{Lexer{IO_t,K}}) where {IO_t,K} = Token{K}

function Base.iterate(l::Lexer)
    seekstart(l)
    l.token_startpos = position(l)

    l.current_pos = l.io_startpos
    t = l.next_token(l)
    return t, l.chars[1] == EOF_CHAR
end

function Base.iterate(l::Lexer, isdone::Any)
    isdone && return nothing
    t = l.next_token(l)
    return t, l.chars[1] == EOF_CHAR
end

function Base.show(io::IO, l::Lexer)
    print(io, typeof(l), " at position: ", position(l))
end

"""
    startpos(l::Lexer)

Return the latest `Token`'s starting position.
"""
startpos(l::Lexer) = l.token_startpos


Base.seekstart(l::Lexer) = seek(l.io, l.io_startpos)

"""
    seek2startpos!(l::Lexer)

Sets the lexer's current position to the beginning of the latest `Token`.
"""
seek2startpos!(l::Lexer) = seek(l, startpos(l))

"""
    peekchar(l::Lexer)

Returns the next character without changing the lexer's state.
"""
peekchar(l::Lexer) = l.chars[2]

"""
dpeekchar(l::Lexer)

Returns the next two characters without changing the lexer's state.
"""
dpeekchar(l::Lexer) = l.chars[2], l.chars[3]

"""
    position(l::Lexer)

Returns the current position.
"""
Base.position(l::Lexer) = l.charspos[1]

"""
    eof(l::Lexer)

Determine whether the end of the lexer's underlying buffer has been reached.
"""# Base.position(l::Lexer) = Base.position(l.io)
eof(l::Lexer) = eof(l.io)

function Base.seek(l::Lexer, pos)
    seek(l.io, pos)
    l.token_startpos = l.current_pos = pos
    dr = l.doread
    l.doread = false
    for i = 1:2
        # Clear char cache
        readchar(l)
    end
    return nothing
end

"""
    start_token!(l::Lexer)

Updates the lexer's state such that the next  `Token` will start at the current
position.
"""
function start_token!(l::Lexer)
    l.token_startpos = l.charspos[1]
end

"""
    readchar(l::Lexer)

Returns the next character and increments the current position.
"""
function readchar end

function readchar(l::Lexer{I}) where {I <: IO}
    # SPICE is case insensitive
    c = readchar(l.io)
    if isvalid(c) && (!l.case_sensitive)
        c2 = uppercase(c)
        @assert ncodeunits(c) == ncodeunits(c2)
        c = c2
    end
    l.chars = (l.chars[2], l.chars[3], c)
    l.charspos = (l.charspos[2], l.charspos[3], position(l.io))
    if l.doread
        write(l.charstore, l.chars[1])
    end
    return l.chars[1]
end

"""
    accept(l::Lexer, f::Union{Function, Char, Vector{Char}, String})

Consumes the next character `c` if either `f::Function(c)` returns true, `c == f`
for `c::Char` or `c in f` otherwise. Returns `true` if a character has been
consumed and `false` otherwise.
"""
@inline function accept(l::Lexer, f::Union{Function, Char, Vector{Char}, String})
    c = peekchar(l)
    eof(c) && return false
    if isa(f, Function)
        ok = f(c)
    elseif isa(f, Char)
        ok = c == f
    else
        ok = c in f
    end
    ok && readchar(l)
    return ok
end

"""
    accept_batch(l::Lexer, f)

Consumes all following characters until `accept(l, f)` is `false`.
"""
@inline function accept_batch(l::Lexer, f)
    ok = false
    while accept(l, f)
        ok = true
    end
    return ok
end

"""
    emit(l::Lexer, kind::Kind)

Returns a `Token` of kind `kind` with contents `str` and starts a new `Token`.
"""
function emit(l::Lexer{IO_t}, kind) where IO_t
    tok = Token(kind,
                  startpos(l), position(l) - 1)
    l.last_token = kind
    if !is_triv(kind)
        l.lexed_nontriv_token_line = true
        l.last_nontriv_token = kind
    end
    if is_newline(kind)
        l.lexed_nontriv_token_line = false
    end
    start_token!(l)
    return tok
end
