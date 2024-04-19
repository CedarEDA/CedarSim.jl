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
    IDENTIFIER, # foo, Σxx
    BASE_SPEC, # 'b (but not hex)
    HEX_BASE_SPEC, # 'h or 'H
    BACKTICK, # `
    COMMA, #,
    COLON, #:
    SEMICOLON, # ;
    EQ, # =
    DOT, # .
    AT_SIGN, # @
    HASH, # #
    DOLLAR, # $
    CONDITIONAL, # ?
    PRIME, # '

    INCLUDE_FNAME,
    JULIA_ESCAPE, # $( if enabled

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
        # TODO Classify these
        CORRELATE,
        ELSE,
        END,
        ENDS,
        EXPORT,
        FOR,
        FUNCTION,
        GLOBAL,
        IF,
        INLINE,
        LIBRARY,
        LOCAL,
        MARCH,
        MODEL,
        PARAMETERS,
        PARAMTEST,
        PLOT,
        PRINT,
        REAL,
        RETURN,

        SUBCKT,

        TO,
        VARY,

        SIMULATOR,
        LANG,
        SPECTRE,
        SPICE,
        INCLUDE,
        AHDL_INCLUDE,

        begin_control,
            begin_second_control,
                ALTER,
                ALTERGROUP,
                ASSERT,
                CHECK,
                CHECKLIMIT,
                INFO,
                OPTIONS,
                PARAMSET,
                SET,
                SHELL,
            end_second_control,
            begin_first_control,
                IC,
                NODESET,
                SAVE,
            end_first_control,
            STATISTICS,
        end_control,

        begin_analyses,
            DC,
            AC,
            NOISE,
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
        end_analyses,

        SECTION,

        begin_builtin_constants,
            M_1_PI,
            M_2_PI,
            M_2_SQRTP,
            M_DEGPERRAD,
            M_E,
            M_LN10,
            M_LN2,
            M_LOG10E,
            M_LOG2E,
            M_PI,
            M_PI_2,
            M_PI_4,
            M_SQRT1_2,
            M_SQRT2,
            M_TWO_PI,
            P_C,
            P_CELSIUS0,
            P_EPS0,
            P_H,
            P_K,
            P_Q,
            P_U0,
        end_builtin_constants,

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
            NINT,
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

        begin_save_keywords,
            CURRENTS,
            STATIC,
            DISPLACEMENT,
            DYNAMIC,
            OPPOINT,
            PROBE,
            PWR,
            ALL,
        end_save_keywords,
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
is_ident(k::Kind) = k == IDENTIFIER || is_kw(k)
Lexers.is_triv(k::Kind) = k == COMMENT || k == WHITESPACE || k == NEWLINE
Lexers.is_newline(k::Kind) = k == NEWLINE
is_builtin_func(k::Kind) = begin_builtin_functions < k < end_builtin_functions
is_builtin_const(k::Kind) = begin_builtin_constants < k < end_builtin_constants
is_analysis(k::Kind) = begin_analyses < k < end_analyses
is_save_kw(k::Kind) = begin_save_keywords < k < end_save_keywords
is_literal(k::Kind) = begin_literal < k < end_literal
is_number(k::Kind) = begin_number < k < end_number
is_operator(k::Kind) = begin_ops < k < end_ops
is_unary_operator(k::Kind) = k == PLUS || k == MINUS

const reserved_words = Dict{String, Kind}()
for k in instances(Kind)
    is_kw(k) || continue
    all(x->isuppercase(x)||x=='_', string(k)) || continue
    str = is_builtin_const(k) ? string(k) : lowercase(string(k))
    reserved_words[str] = k
end
