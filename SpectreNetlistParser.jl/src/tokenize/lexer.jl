include("token_kinds.jl")

export tokenize

function is_identifier_char(c::Char)
    return isletter(c) || isdigit(c) || c == '_' || c == '$' || c == '!'
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
function next_token(l::Lexer{IO, K})::Token{K} where {IO, K}
    c = Lexers.readchar(l)

    if is_literal(l.last_token)
        lexed_unit = maybe_lex_unit(l, c)
        if lexed_unit
            return emit(l, UNIT)
        end
    end

    if eof(c);
        return emit(l, ENDMARKER)
    elseif c == '\n' || c == '\r'
        return lex_newline(l, c)
    elseif iswhitespace(c)
        return lex_whitespace(l)
    elseif c == '"'
        return lex_quote(l);
    elseif c == '/'
        return lex_slash(l)
    elseif c == '\\'
        return lex_backslash(l)
    elseif c == '$'
        return lex_identifier(l, c)
    elseif c == '`'
        return emit(l, BACKTICK)
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
        return emit(l, PERCENT);
    elseif c == '.'
        return lex_dot(l)
    elseif c == '+'
        return emit(l, PLUS)
    elseif c == '-'
        return emit(l, MINUS)
    elseif ishex(c) && l.last_nontriv_token == HEX_BASE_SPEC
        return lex_hex(l)
    elseif is_identifier_start_char(c)
        return lex_identifier(l, c)
    elseif isdigit(c)
        return lex_digit(l, INT_LIT)
    else
        emit(l, UNKNOWN)
    end
end


function lex_include_fname(l::Lexer)
    accept_batch(l, !=('"')) # TODO: stop seraching at newline?
    return emit(l, INCLUDE_FNAME)
end

function lex_paren(l::Lexer)
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

function lex_slash(l::Lexer)
    if peekchar(l) == '/'
        # Line comment
        while true
            accept_batch(l, !=('\n'))
            return emit(l, COMMENT)
        end
    else
        return emit(l, SLASH)
    end
end

function lex_newline(l::Lexer, c)
    if c == '\n'
        return emit(l, NEWLINE)
    elseif c == '\r'
        if accept(l, '\n')
            return emit(l, NEWLINE)
        else
            return emit(l, WHITESPACE)
        end
    end
end

function lex_backslash(l::Lexer)
    if accept(l, '\n')
        return emit(l, ESCD_NEWLINE)
    elseif accept(l, '\r') && accept(l, '\n')
        return emit(l, ESCD_NEWLINE)
    end
    return lex_escaped_identifier(l)
end

# Lex a greater char, a '>' has been consumed
function lex_greater(l::Lexer)
    if accept(l, '>') # >>
        if accept(l, '>') # >>>
            return emit(l, RRBITSHIFT_A)
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
            return emit(l, LBITSHIFT_A)
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

function lex_dot(l::Lexer)
    if Base.isdigit(peekchar(l))
        return lex_digit(l, FLOAT)
    else
        return emit(l, DOT)
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
            return emit(l, NOT_IS)
        else # !=
            return emit(l, NOT_EQ)
        end
    else
        return emit(l, NOT)
    end
end

function lex_star(l::Lexer)
    if l.last_token == NEWLINE
        accept_batch(l, !=('\n'))
        return emit(l, COMMENT)
    end
    if accept(l, '*')
        return emit(l, STAR_STAR)
    end
    return emit(l, STAR)
end

function lex_circumflex(l::Lexer)
    if accept(l, '~')
        return emit(l, XOR_TILDE)
    end
    return emit(l, XOR)
end

function lex_tilde(l::Lexer)
    if accept(l, '^')
        return emit(l, TILDE_XOR)
    elseif accept(l, '&')
        return emit(l, TILDE_AND)
    elseif accept(l, '|')
        return emit(l, TILD_OR)
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

is_scale_factor(c) = c in ('T', 'G', 'M', 'K', 'k', '_', '%', 'c', 'm', 'u', 'n', 'p', 'f', 'a')
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

is_single_letter_unit(c) = c in ('V', 'f', 'H', 's')
function maybe_lex_unit(l, c)
    if c == 'H' && accept(l, 'z')
        return true
    elseif is_single_letter_unit(c)
        if isspace(peekchar(l)) || eof(peekchar(l))
            return true
        end
    elseif c == 'O' && accept(l, 'h') && accept(l, 'm') && accept(l, 's')
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

function lex_prime(l)
    return emit(l, PRIME)
end

function lex_amper(l::Lexer)
    if accept(l, '&')
        return emit(l, LAZY_AND)
    else
        return emit(l, AND)
    end
end

function lex_bar(l::Lexer)
    if accept(l, '|')
        return emit(l, LAZY_OR)
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


const KW_TRIE = Trie(reserved_words)

function lex_identifier(l::Lexer{IO_t,T}, c) where {IO_t,T}
    t = subtrie(KW_TRIE, c)
    while true
        pc = peekchar(l)
        if is_identifier_char(pc)
            readchar(l)
            if t !== nothing
                t = subtrie(t, pc)
            end
        else
            break
        end
    end

    if t !== nothing && t.is_key
        return emit(l, t.value)
    end

    return emit(l, IDENTIFIER)
end
