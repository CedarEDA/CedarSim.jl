include("token_kinds.jl")

export tokenize

# https://cseweb.ucsd.edu/classes/wi10/cse241a/assign/hspice_sa.pdf pg 42
const instance_special_start_characters = ('~', '!', '@', '#', '%', '^', '&', '_', '<', '>', '?', '/', '|')
const instance_special_characters = ('$', '*', '-', '+', '{', '}', '[', ']', '\\', ';', ':')

const special_characters = ('~', '*', '-', '+', '<', '>', '?', '/', '{' , '}', '[', ']' , '|', '.', ':', )

function is_instance_start_char(c::Char)
    return isletter(c) || isdigit(c) || c in instance_special_start_characters
end

function is_instance_char(c::Char)
    return isletter(c) || isdigit(c) || c in instance_special_start_characters || c in instance_special_characters
end

function is_identifier_char(c::Char)
    return isletter(c) || isdigit(c) || c == '_' || c == '$' || c == '#'
    # lets be a bit conservative here because some of these are used in expressions
    #|| c == '[' || c == ']' || c == '|' || c == '!'
end

function is_identifier_start_char(c::Char)
    return isletter(c) || c == '_'
end

is_instance_first_char(c) = c in ('B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X')

@inline ishex(c::Char) = isdigit(c) || ('a' <= c <= 'f') || ('A' <= c <= 'F')
@inline isbinary(c::Char) = c == '0' || c == '1'
@inline isoctal(c::Char) =  '0' ≤ c ≤ '7'
@inline is_whitespace(c::Char) = Base.isspace(c) && c != '\n'
@inline is_token_seperator(c::Char) = (Base.isspace(c) || c in (',', '(', ')')) && c != '\n'

function lastis(vec::Vector{T}, val::T) where T
    !isempty(vec) && vec[end] == val
end

"""
    next_token(l::Lexer)

Returns the next `Token`.
"""
function next_token(l::Lexer{IO, K}) where {IO, K}
    if l.last_nontriv_token == TITLE
        accept_batch(l, ∉(('\n', '\r')))
        return emit(l, TITLE_LINE)
    end

    c = readchar(l)

    if is_number(l.last_token)
        lexed_unit = maybe_lex_unit(l, c)
        if lexed_unit
            return emit(l, UNIT)
        end
    end

    # if the first nontrivial token is not a plus (continuation)
    # clear the stack of expression delimiters
    if !l.lexed_nontriv_token_line && !is_whitespace(c) && c != '+'
        empty!(l.lexing_expression_stack)
    end

    if eof(c);
        return emit(l, ENDMARKER)
    elseif c == '\n' || c == '\r'
        return lex_newline(l, c)
    elseif ((!isempty(l.lexing_expression_stack) && is_whitespace(c)) ||
           (isempty(l.lexing_expression_stack) && is_token_seperator(c)))
        return lex_whitespace(l)
    elseif l.last_nontriv_token ∈ (INCLUDE, LIB, HDL)
        return lex_path(l, c)
    elseif !l.lexed_nontriv_token_line && is_instance_first_char(c)
        return lex_instance(l, c)
    elseif c == '/'
        return lex_slash(l)
    elseif c == '\\'
        return lex_backslash(l)
    elseif c == '$'
        return lex_dollar(l)
    elseif c == '`'
        return emit(l, BACKTICK)
    elseif c == '*'
        return lex_star(l)
    elseif c == '['
        return emit(l, LSQUARE)
    elseif c == ']'
        return emit(l, RSQUARE)
    elseif c == '{'
        push!(l.lexing_expression_stack, LBRACE)
        return emit(l, LBRACE)
    elseif c == ';'
        return lex_semicolon(l)
    elseif c == '}'
        return lex_rbrace(l)
    elseif c == '('
        push!(l.lexing_expression_stack, LPAREN)
        return emit(l, LPAREN)
    elseif c == ')'
        return lex_rparen(l);
    elseif c == ','
        return lex_comma(l);
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
    elseif c == '"'
        return lex_prime(l);
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

function lex_rbrace(l::Lexer)
    @assert lastis(l.lexing_expression_stack, LBRACE)
    pop!(l.lexing_expression_stack)
    if lastis(l.lexing_expression_stack, EQ)
        pop!(l.lexing_expression_stack)
    end
    return emit(l, RBRACE)
end

function lex_comma(l::Lexer)
    # if this comma would end an implicit expression, threat it as a token seperator
    # note that if an LPAREN was consumed, it would lex as COMMA (probably fn arg seperator)
    if lastis(l.lexing_expression_stack, EQ)
        pop!(l.lexing_expression_stack)
        return lex_whitespace(l)
    else
        return emit(l, COMMA)
    end
end

function lex_path(l::Lexer, c::Char)
    if c ∈ ('"', '\'')
        return lex_quote(l)
    else
        accept_batch(l, !isspace)
        # we can't tell the difference between `.lib path section` and `.lib section`
        # so we must assume this is an identifier and have the parser deal with it.
        return emit(l, IDENTIFIER)
    end
end

function lex_rparen(l::Lexer)
    if lastis(l.lexing_expression_stack, LPAREN) || lastis(l.lexing_expression_stack, JULIA_ESCAPE_BEGIN)
        pop!(l.lexing_expression_stack)
        return emit(l, RPAREN)
    else
        return lex_whitespace(l)
    end
end

# Lex whitespace, a whitespace char has been consumed
function lex_whitespace(l::Lexer)
    if !isempty(l.lexing_expression_stack)
        # we're in an expression, don't accept "token seperators"
        accept_batch(l, is_whitespace)
        # if we're in an implicit expression `r = foo+bar`
        # it is terminated by whitespace AFTER the expression started
        # therefore don't pop the EQ if the last nontrivial token was the EQ itself.
        if lastis(l.lexing_expression_stack, EQ) &&
           l.last_nontriv_token != EQ
            pop!(l.lexing_expression_stack)
        end
    else
        accept_batch(l, is_token_seperator)
    end
    # lexing whitespace after an equals sign ends expression mode
    # unless we're in some other type of expression like a prime or brace
    return emit(l, WHITESPACE)
end

# Lex identifier after an escaping backslash.
function lex_escaped_identifier(l::Lexer)
    if !isempty(l.lexing_expression_stack)
        accept_batch(l, !is_whitespace)
    else
        accept_batch(l, !is_token_seperator)
    end
    return emit(l, IDENTIFIER)
end

function lex_slash(l::Lexer)
    return emit(l, SLASH)
end

function lex_newline(l::Lexer, c)
    if lastis(l.lexing_expression_stack, EQ)
        pop!(l.lexing_expression_stack)
    end
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
    if is_ident(l.last_token)
        return emit(l, DOT)
    end
    if Base.isdigit(peekchar(l))
        return lex_digit(l, FLOAT)
    else
        t = emit(l, DOT)
        return t
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
        push!(l.lexing_expression_stack, EQ)
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

function lex_dollar(l::Lexer)
    if l.enable_julia_escape && accept(l, '(')
        push!(l.lexing_expression_stack, JULIA_ESCAPE_BEGIN)
        return emit(l, JULIA_ESCAPE_BEGIN)
    end
    accept_batch(l, !=('\n')) # TODO: '\r'
    return emit(l, COMMENT)
end

function lex_semicolon(l::Lexer)
    accept_batch(l, !=('\n')) # TODO: '\r'
    return emit(l, COMMENT)
end

function lex_star(l::Lexer)
    if !l.lexed_nontriv_token_line
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

is_scale_factor(c) = c in ('T', 'G', 'K', 'M', 'U', 'N', 'P', 'F', 'A')
function accept_float(l, pc, ppc)
    ok = false
    if (pc == 'e' || pc == 'E') && (isdigit(ppc) || ppc == '+' || ppc == '-')
        readchar(l)
        accept(l, "+-")
        accept_number(l, isdigit)
        ok = true
    end
    pc, ppc = dpeekchar(l)
    if is_scale_factor(pc) && (pc, ppc) != ('A', 'M'#=, 'P'=#)
        readchar(l)
        if dpeekchar(l) == ('E', 'G') || dpeekchar(l) == ('I', 'L')
            readchar(l)
            readchar(l)
        end
        ok = true
    end
    return ok
end

function maybe_lex_unit(l, c)
    unit = isletter(c)
    while unit && isletter(peekchar(l))
        readchar(l)
        unit = true
    end
    return unit
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
    if l.last_nontriv_token ∈ (LIB, INCLUDE)
        return lex_quote(l)
    end
    if lastis(l.lexing_expression_stack, PRIME)
        pop!(l.lexing_expression_stack)
        if lastis(l.lexing_expression_stack, EQ)
            pop!(l.lexing_expression_stack)
        end
    else
        push!(l.lexing_expression_stack, PRIME)
    end
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
    if accept(l, '"') || accept(l, '\'') # ""
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
    if kind == STRING && (l.chars[1] == '"' || l.chars[1] == '\'')
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
        # if we're not inside an expression, be more liberal in accepting names
        # for example V- and V+ are fine node names but 'V+foo' is V PLUS foo.
        if (is_identifier_char(pc) && !isempty(l.lexing_expression_stack)) ||
            (is_instance_char(pc) && isempty(l.lexing_expression_stack))
            readchar(l)
            if t !== nothing
                t = subtrie(t, pc)
            end
        else
            break
        end
    end

    if t !== nothing
        while length(t.children)==1
            t = first(values(t.children))
        end
        if t.is_key
            # some dot commands are implicit expressions
            if l.last_nontriv_token == DOT && t.value in (PARAMETERS, IC, MEASURE, PRINT)
                push!(l.lexing_expression_stack, t.value)
            end
            return emit(l, t.value)
        end
    end

    return emit(l, IDENTIFIER)
end

function lex_instance(l::Lexer{IO_t,T}, c) where {IO_t,T}
    typ = if c == 'D'
        IDENTIFIER_DIODE
    elseif c == 'B'
        IDENTIFIER_BEHAVIORAL
    elseif c == 'C'
        IDENTIFIER_CAPACITOR
    elseif c == 'E'
        IDENTIFIER_VOLTAGE_CONTROLLED_VOLTAGE
    elseif c == 'F'
        IDENTIFIER_CURRENT_CONTROLLED_CURRENT
    elseif c == 'G'
        IDENTIFIER_VOLTAGE_CONTROLLED_CURRENT
    elseif c == 'H'
        IDENTIFIER_CURRENT_CONTROLLED_VOLTAGE
    elseif c == 'I'
        IDENTIFIER_CURRENT
    elseif c == 'J'
        IDENTIFIER_JFET
    elseif c == 'K'
        IDENTIFIER_LINEAR_MUTUAL_INDUCTOR
    elseif c == 'L'
        IDENTIFIER_LINEAR_INDUCTOR
    elseif c == 'M'
        IDENTIFIER_MOSFET
    elseif c == 'P'
        IDENTIFIER_PORT
    elseif c == 'Q'
        IDENTIFIER_BIPOLAR_TRANSISTOR
    elseif c == 'R'
        IDENTIFIER_RESISTOR
    elseif c == 'S' && l.spice_dialect == :ngspice
        IDENTIFIER_SWITCH
    elseif c == 'S' && l.spice_dialect == :hspice
        IDENTIFIER_S_PARAMETER_ELEMENT
    elseif c == 'V'
        IDENTIFIER_VOLTAGE
    elseif c == 'T' || c == 'U' || c == 'W'
        IDENTIFIER_TRANSMISSION_LINE
    elseif c == 'X'
        IDENTIFIER_SUBCIRCUIT_CALL
    else
        error("unhandled first character in instance $(repr(c))")
    end

    if accept(l, is_instance_start_char)
        accept_batch(l, is_instance_char)

        if typ ∈ (IDENTIFIER_S_PARAMETER_ELEMENT, IDENTIFIER_SWITCH)
            range = Lexers.startpos(l):Lexers.position(l) - 1
            # It is possible that we have a "simulator" statement here.
            # TODO: Make this more efficient?
            if lowercase(String(l.io.data[1 .+ range])) == "simulator"
                return emit(l, SIMULATOR)
            end
        end
        return emit(l, typ)
    end
    emit(l, ERROR)
end
