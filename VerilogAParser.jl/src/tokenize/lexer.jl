include("token_kinds.jl")
export tokenize

function is_identifier_char(c::Char)
    return isletter(c) || isdigit(c) || c == '_' || c == '$'
end

function is_identifier_start_char(c::Char)
    # CN: This allows unicode letters, though the standard does not specify
    # whether or not this is permitted.
    return isletter(c) || c == '_'
end

@inline ishex(c::Char) = isdigit(c) || ('a' <= c <= 'f') || ('A' <= c <= 'F')
@inline isbinary(c::Char) = c == '0' || c == '1'
@inline isoctal(c::Char) =  '0' ≤ c ≤ '7'
@inline iswhitespace(c::Char) = Base.isspace(c) && c != '\n'

"""
    next_token(l::Lexer)

Returns the next `Token`.
"""
function next_token(l::Lexer, start = true)
    # It is possible for this token to be empty - don't consume a char
    if l.last_token in (M_INCLUDE_TRIVIA_ANGLE, M_INCLUDE_TRIVIA_QUOTE)
        return lex_include_fname(l,
            l.last_token == M_INCLUDE_TRIVIA_ANGLE ? '>' : '"')
    end

    c = readchar(l)
    if eof(c);
        return emit(l, ENDMARKER)
    elseif c == '\n'
        return emit(l, NEWLINE)
    elseif l.last_token == M_INCLUDE
        return lex_include_start_trivia(l)
    elseif l.last_token == M_INCLUDE_FNAME
        return lex_include_end_trivia(l, c)
    elseif iswhitespace(c)
        return lex_whitespace(l)
    elseif c == '"'
        return lex_quote(l);
    elseif c == '/'
        return lex_slash(l)
    elseif c == '\\'
        return lex_backslash(l)
    elseif c == '$'
        return lex_system_ident(l)
    elseif c == '`'
        if peekchar(l) == '`'
            readchar(l)
            return emit(l, BACKTICK_BACKTICK)
        elseif peekchar(l) == '`'
            readchar(l)
            return emit(l, BACKTICK_QUOTE)
        else
            return emit(l, BACKTICK)
        end
    elseif c == '('
        return lex_paren(l)
    elseif c == '*'
        return lex_star(l)
    elseif c == '['
        return emit(l, LSQUARE)
    elseif c == ']'
        return emit(l, RSQUARE)
    elseif c == '{'
        return emit(l, LBRACE)
    elseif c == ';'
        return emit(l, SEMICOLON)
    elseif c == '}'
        return emit(l, RBRACE)
    elseif c == '('
        return emit(l, LPAREN)
    elseif c == ')'
        return emit(l, RPAREN)
    elseif c == ','
        return emit(l, COMMA)
    elseif c == '^'
        return lex_circumflex(l);
    elseif c == '~'
        return lex_tilde(l)
    elseif c == '@'
        return emit(l, AT_SIGN)
    elseif c == '?'
        return emit(l, CONDITIONAL)
    elseif c == '='
        return lex_equal(l)
    elseif c == '!'
        return lex_exclaim(l)
    elseif c == '>'
        return lex_greater(l)
    elseif c == '<'
        return lex_less(l)
    elseif c == ':'
        return emit(l, COLON)
    elseif c == '&'
        return lex_amper(l)
    elseif c == '|'
        return lex_bar(l)
    elseif c == '\''
        return lex_prime(l)
    elseif c == '%'
        if accept(l, '%')
            return emit(l, PERCENT_PERCENT)
        elseif accept(l, '=')
            return emit(l, PERCENT_EQ)
        else
            return emit(l, PERCENT)
        end
    elseif c == '.'
        return emit(l, DOT)
    elseif c == '+'
        if accept(l, '=')
            return emit(l, PLUS_EQ)
        else
            return emit(l, PLUS)
        end
    elseif c == '-'
        if accept(l, '=')
            return emit(l, MINUS_EQ)
        else
            return emit(l, MINUS)
        end
    elseif ishex(c) && l.last_nontriv_token == HEX_BASE_SPEC
        return lex_hex(l)
    elseif is_identifier_start_char(c)
        return lex_identifier(l, c)
    elseif isdigit(c)
        return lex_digit(l, INT)
    else
        emit(l, UNKNOWN)
    end
end

function lex_include_fname(l::Lexer, closer::Char)
    accept_batch(l, x->x != closer)
    return emit(l, M_INCLUDE_FNAME)
end

function lex_include_end_trivia(l::Lexer, c)
    @assert c in ('>', '"')
    while true
        accept_batch(l, iswhitespace)
        peekchar(l) == '\n' && break
        if !accept_comment(l)
            # Lex through the rest of the line, to potentially allow recovery
            # on the next line
            accept_batch(l, c->c != '\n')
            return emit(l, INCLUDE_DIRECTIVE_JUNK)
        end
    end
    return emit(l, c == '>' ? M_INCLUDE_TRIVIA_ANGLE_CLOSE : M_INCLUDE_TRIVIA_QUOTE_CLOSE)
end

function lex_include_start_trivia(l::Lexer)
    accept_batch(l, iswhitespace)
    c = peekchar(l)
    if accept(l, "<\"")
        return emit(l, c == '<' ? M_INCLUDE_TRIVIA_ANGLE : M_INCLUDE_TRIVIA_QUOTE)
    else
        return emit(l, UNKNOWN)
    end
end

function lex_paren(l::Lexer)
    if peekchar(l) == '*'
        readchar(l)
        return emit(l, LATTR)
    end
    return emit(l, LPAREN)
end

# Lex whitespace, a whitespace char has been consumed
function lex_whitespace(l::Lexer)
    accept_batch(l, iswhitespace)
    return emit(l, WHITESPACE)
end

# Lex identifier after an escaping backslash.
function lex_escaped_identifier(l::Lexer)
    accept_batch(l, !iswhitespace)
    return emit(l, IDENTIFIER)
end

is_system_identifier_char(c) = isdigit(c) || 'a' <= c <= 'z' || 'A' <= c <= 'Z' || c in ('$', '_')
function lex_system_ident(l::Lexer)
    accept_batch(l, is_system_identifier_char)
    return emit(l, SYSTEM_IDENTIFIER)
end

function lex_slash(l::Lexer, ::Val{doemit} = Val(true)) where {doemit}
    if peekchar(l) == '/'
        # Line comment
        while true
            pc = peekchar(l)
            if pc == '\n' || eof(pc)
                return doemit ? emit(l, COMMENT) : COMMENT
            end
            readchar(l)
        end
    elseif peekchar(l) == '*'
        c = readchar(l) # consume the '*'
        if eof(c)
            return doemit ? emit(l, EOF_MULTICOMMENT) : ERROR
        end
        c = readchar(l)
        while true
            if eof(c)
                return doemit ? emit(l, EOF_MULTICOMMENT) : ERROR
            end
            nc = readchar(l)
            if c == '*' && nc == '/'
                return doemit ? emit(l, COMMENT) : COMMENT
            end
            c = nc
        end
    elseif peekchar(l) == '='
        doemit && readchar(l)
        return doemit ? emit(l, SLASH_EQ) : SLASH_EQ
    else
        doemit ? emit(l, SLASH) : SLASH
    end
end

function accept_comment(l::Lexer)
    peekchar(l) == '/' || return false
    readchar(l)
    # TODO: This probably consumes more than it should
    return lex_slash(l, Val(false)) == COMMENT
end

function lex_backslash(l::Lexer)
    if accept(l, '\n')
        return emit(l, ESCD_NEWLINE)
    end
    return lex_escaped_identifier(l)
end

# Lex a greater char, a '>' has been consumed
function lex_greater(l::Lexer)
    if accept(l, '>') # >>
        if accept(l, '>') # >>>
            if accept(l, '=')
                return emit(l, RBITSHIFT_A_EQ)
            end
            return emit(l, RBITSHIFT_A)
        elseif accept(l, '=') # >>=
            return emit(l, RBITSHIFT_EQ)
        else # '>>'
            return emit(l, RBITSHIFT)
        end
    elseif accept(l, '=') # '>=
        return emit(l, GREATER_EQ)
    else  # '>'
        return emit(l, GREATER)
    end
end

# Lex a less char, a '<' has been consumed
function lex_less(l::Lexer)
    if accept(l, '<') # <<
        if accept(l, '<') # <<<
            if accept(l, '=')
                return emit(l, LBITSHIFT_A_EQ)
            end
            return emit(l, LBITSHIFT_A)
        elseif accept(l, '=') # '<<='
            return emit(l, LBITSHIFT_EQ)
        else
            return emit(l, LBITSHIFT)
        end
    elseif accept(l, '=') # <=
        return emit(l, LESS_EQ)
    elseif accept(l, '+')
        return emit(l, CASSIGN)
    else
        return emit(l, LESS) # '<'
    end
end

# Lex all tokens that start with an = character.
# An '=' char has been consumed
function lex_equal(l::Lexer)
    if accept(l, '=') # ==
        if accept(l, '=') # ===
            emit(l, EQEQEQ)
        else
            emit(l, EQEQ)
        end
    else
        emit(l, EQ)
    end
end

function lex_exclaim(l::Lexer)
    if accept(l, '=') # !=
        if accept(l, '=') # !==
            return emit(l, NOT_EQEQ)
        else # !=
            return emit(l, NOT_EQ)
        end
    else
        return emit(l, NOT)
    end
end

function lex_star(l::Lexer)
    if accept(l, '*')
        if accept(l, '=') # **=
            return emit(l, STAR_STAR_EQ)
        end
        return emit(l, STAR_STAR)
    elseif accept(l, ')')
        return emit(l, RATTR)
    elseif accept(l, '=')
        return emit(l, STAR_EQ)
    end
    return emit(l, STAR)
end

function lex_circumflex(l::Lexer)
    if accept(l, '~')
        return emit(l, XOR_TILDE)
    elseif accept(l, '=')
        return emit(l, XOR_EQ)
    end
    return emit(l, XOR)
end

function lex_tilde(l::Lexer)
    if accept(l, '^')
        return emit(l, TILDE_XOR)
    elseif accept(l, '&')
        return emit(l, TILDE_AND)
    elseif accept(l, '|')
        return emit(l, TILDE_OR)
    end
    return emit(l, TILDE)
end

function accept_number(l::Lexer, f::F) where {F}
    while true
        pc, ppc = dpeekchar(l)
        if pc == '_' && !f(ppc)
            return
        elseif f(pc) || pc == '_'
            readchar(l)
        else
            return
        end
    end
end

is_scale_factor(c) = c in ('T', 'G', 'M', 'K', 'k', 'm', 'n', 'u', 'n', 'p', 'f', 'a')
function accept_float(l, pc, ppc)
    if (pc == 'e' || pc == 'E') && (isdigit(ppc) || ppc == '+' || ppc == '-')
        readchar(l)
        accept(l, "+-")
        accept_number(l, isdigit)
        return true
    elseif is_scale_factor(pc)
        readchar(l)
        return true
    end
    return false
end

# A digit has been consumed
function lex_digit(l::Lexer, kind)
    accept_number(l, isdigit)
    pc, ppc = dpeekchar(l)
    if pc == '.'
        readchar(l)
        kind = FLOAT
        accept_number(l, isdigit)
        accept_float(l, dpeekchar(l)...)
    elseif accept_float(l, pc, ppc)
        kind = FLOAT
    end
    return emit(l, kind)
end

function lex_hex(l::Lexer)
    accept_number(l, ishex)
    return emit(l, HEX_INT)
end

is_signed_char(c) = c in ('s', 'S')
is_hex_base_char(c) = c in ('h', 'H')
is_base_char(c) = c in ('d', 'D', 'h', 'H', 'o', 'O', 'b', 'B')

function lex_prime(l, doemit = true)
    if accept(l, '{')
        return emit(l, AP_LBRACE)
    end
    accept(l, is_signed_char)
    if accept(l, is_hex_base_char)
        return emit(l, HEX_BASE_SPEC)
    elseif accept(l, is_base_char)
        return emit(l, BASE_SPEC)
    end
    return emit(l, INVALID_BASE_SPEC)
end

function lex_amper(l::Lexer)
    if accept(l, '&')
        return emit(l, LAZY_AND)
    elseif accept(l, '=')
        return emit(l, AND_EQ)
    else
        return emit(l, AND)
    end
end

function lex_bar(l::Lexer)
    if accept(l, '|')
        return emit(l, LAZY_OR)
    elseif accept(l, '=')
        return emit(l, OR_EQ)
    else
        return emit(l, OR)
    end
end

# Parse a token starting with a quote.
# A '"' has been consumed
function lex_quote(l::Lexer)
    if accept(l, '"') # ""
        # empty string
        return emit(l, STRING)
    else
        if read_string(l, STRING)
            return emit(l, STRING)
        else
            return emit(l, EOF_STRING)
        end
    end
end

function string_terminated(l, kind::Kind)
    if kind == STRING && l.chars[1] == '"'
        return true
    end
    return false
end

# We just consumed a ", """, `, or ```
function read_string(l::Lexer, kind::Kind)
    while true
        c = readchar(l)
        if c == '\\'
            eof(readchar(l)) && return false
            continue
        end
        if string_terminated(l, kind)
            return true
        elseif eof(c)
            return false
        end
    end
end

function tryread(l, str, k)
    for s in str
        c = peekchar(l)
        if c != s
            if !is_identifier_char(c)
                return emit(l, IDENTIFIER)
            end
            return readrest(l, c)
        else
            readchar(l)
        end
    end
    c = peekchar(l)
    if is_identifier_char(c)
        return readrest(l, c)
    end
    return emit(l, k)
end

function readrest(l, c, tt=IDENTIFIER)
    while true
        pc = peekchar(l)
        if !is_identifier_char(pc)
            break
        end
        c = readchar(l)
    end

    return emit(l, tt)
end


function _doret(l, c, thistt=IDENTIFIER)
    if !is_identifier_char(c)
        return emit(l, thistt)
    else
        return readrest(l, c)
    end
end

include("keywords.jl")
