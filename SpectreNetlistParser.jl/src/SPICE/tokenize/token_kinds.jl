module Tokens

macro exported_enum(x...)
    top = macroexpand(__module__, :(@enum($(x...))))
    top.args[end] = :(export $(x...))
    push!(top.args, nothing)
    esc(top)
end

@exported_enum(Kind,
    ENDMARKER, # EOF
    COMMENT, # //
    WHITESPACE, # '   \t'
    NEWLINE, # \n
    ESCD_NEWLINE, # \ \n
    TITLE_LINE, # .title <TITLE_LINE> (or implicit .TITLE)
    JULIA_ESCAPE_BEGIN, # $( if enabled
    JULIA_ESCAPE, # pseudo token for the julia source

    # https://cseweb.ucsd.edu/classes/wi10/cse241a/assign/hspice_sa.pdf page 57
    begin_identifiers,
        IDENTIFIER, # foo, Σxx
        IDENTIFIER_IBIS_BUFFER,
        IDENTIFIER_BEHAVIORAL,
        IDENTIFIER_CAPACITOR,
        IDENTIFIER_DIODE,
        IDENTIFIER_VOLTAGE_CONTROLLED_VOLTAGE,
        IDENTIFIER_VOLTAGE_CONTROLLED_CURRENT,
        IDENTIFIER_CURRENT_CONTROLLED_VOLTAGE,
        IDENTIFIER_CURRENT_CONTROLLED_CURRENT,
        IDENTIFIER_CURRENT,
        IDENTIFIER_JFET,
        IDENTIFIER_LINEAR_MUTUAL_INDUCTOR,
        IDENTIFIER_LINEAR_INDUCTOR,
        IDENTIFIER_MOSFET,
        IDENTIFIER_PORT,
        IDENTIFIER_BIPOLAR_TRANSISTOR,
        IDENTIFIER_RESISTOR,
        IDENTIFIER_S_PARAMETER_ELEMENT,
        IDENTIFIER_SWITCH,
        IDENTIFIER_VOLTAGE,
        IDENTIFIER_TRANSMISSION_LINE,
        IDENTIFIER_SUBCIRCUIT_CALL,
    end_identifiers,

    BASE_SPEC, # 'b (but not hex)
    HEX_BASE_SPEC, # 'h or 'H
    BACKTICK, # `
    COMMA, #,
    COLON, #:
    SEMICOLON, # ;
    DOT, # .
    AT_SIGN, # @
    HASH, # #
    DOLLAR, # $
    CONDITIONAL, # ?
    PRIME, # '

    begin_errors,
        ERROR, # Generic Error
        EOF_STRING,
        UNKNOWN,
    end_errors,

    begin_literal,
        LITERAL, # general
        UNIT, # F,
        begin_number,
            INT_LIT, # 4
            HEX_INT, # 0fff0
            FLOAT, # 3.5, 3.7e+3
        end_number,
        STRING, # "foo"
    end_literal,

    begin_delimiters,
        LSQUARE, # [
        RSQUARE, # [
        LBRACE, # {
        RBRACE, # }
        LPAREN, # (
        RPAREN,  # )
        LATTR, # (*
        RATTR, # *)
    end_delimiters,

    begin_keywords,

        begin_analyses,
            DC,
            AC,
            TRAN,
            IC,
            TF,
            NOISE,
            #=
            XF,
            SP,
            TRAN,
            TDR,
            PZ,

            ENVLP,
            PAC,
            PDISTO,
            PNOISE,
            PSS,
            PXF,

            SENS,
            FOURIER,
            DCMATCH,
            STB,
            SWEEP,
            MONTECARLO,
            =#
        end_analyses,

        begin_output,
            PRINT,
            PLOT,
            MEASURE,

        end_output,

        #SECTION,


        #=
        begin_builtin_functions,
            ABS,
            ACOS,
            ACOSH,
            ASIN,
            ASINH,
            ATAN,
            ATAN2,
            ATANH,
            CEIL,
            COS,
            COSH,
            EXP,
            FLOOR,
            FMOD,
            HYPOT,
            INT,
            LOG,
            LOG10,
            MAX,
            MIN,
            POW,
            SIN,
            SINH,
            SQRT,
            TAN,
            TANH,
            TRUNCATE,
        end_builtin_functions,
        =#

        IF,
        ELSE,
        ELSEIF,
        ENDIF,

        MODEL,
        LIB,
        INCLUDE,
        SUBCKT,
        END,
        ENDL,
        ENDS,
        PARAMETERS,
        CSPARAM,
        OPTIONS,
        TEMP,
        WIDTH,
        GLOBAL,
        DEV,
        LOT,
        SIMULATOR,
        LANG,
        SPECTRE,
        SPICE,
        TITLE,
        DATA,
        ENDDATA,
        ON,
        OFF,
        HDL,

        FIND,
        DERIV,
        WHEN,
        AT,
        TD,
        RISE,
        FALL,
        CROSS,
        LAST,
        AVG,MAX,MIN,PP,RMS,INTEG,
        TRIG, VAL, TARG,

        begin_sources,
            PULSE,
            SIN,
            EXP,
            PWL,
            SFFM,
            AM,
            TRNOISE,
            TRRANDOM,
        end_sources,
    end_keywords,

    begin_ops,
        OP, # general

        # Arithmenic
        PLUS, # +
        MINUS, # -
        STAR, # *
        SLASH, # /
        STAR_STAR, # **

        # Modulus
        PERCENT, # %

        # Relational
        GREATER, # >
        LESS, # <
        GREATER_EQ, # >=
        LESS_EQ, # <=

        # Logical equality
        EQ, # =
        EQEQ, # ==
        NOT_EQ, # !=

        # Case equality
        EQEQEQ, # ===
        NOT_EQEQ, # !==

        # Logical negation
        NOT, # !

        # Logical and
        LAZY_AND, # &&

        # Logical or
        LAZY_OR, # ||

        # Bitwise
        TILDE, # ~
        AND, # &
        OR, # |
        XOR, # ^

        # Reduction
        XOR_TILDE, # ^~
        TILDE_XOR, # ~^
        TILDE_AND, # ~&
        TILDE_OR, # ~|

        # Bitshifts
        LBITSHIFT, # <<
        RBITSHIFT, # >>
        LBITSHIFT_A, # <<<
        RBITSHIFT_A, # >>>

        # Literal "or"
        EVENT_OR, # or
    end_ops,
)


export reserved_words

end
using .Tokens

# It is not trivial to distinguish keywords and identifiers in the lexer so
# do that final distinction in the parser instead
is_kw(k::Kind) =  begin_keywords < k < end_keywords
is_ident(k::Kind) = begin_identifiers < k < end_identifiers || is_kw(k)
Lexers.is_triv(k::Kind) = k == COMMENT || k == WHITESPACE || k == NEWLINE
Lexers.is_newline(k::Kind) = k == NEWLINE
is_builtin_func(k::Kind) = begin_builtin_functions < k < end_builtin_functions
is_analysis(k::Kind) = begin_analyses < k < end_analyses
is_literal(k::Kind) = begin_literal < k < end_literal
is_number(k::Kind) = begin_number < k < end_number
is_operator(k::Kind) = begin_ops < k < end_ops
is_unary_operator(k::Kind) = k == PLUS || k == MINUS
is_source_type(k::Kind) = begin_sources < k < end_sources

const reserved_words = Dict{String, Kind}()
for k in instances(Kind)
    is_kw(k) || continue
    reserved_words[string(k)] = k
end
