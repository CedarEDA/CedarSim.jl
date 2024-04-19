module Tokens

macro exported_enum(x...)
    top = macroexpand(__module__, :(@enum($(x...))))
    top.args[end] = :(export $(x...))
    push!(top.args, nothing)
    esc(top)
end

@exported_enum(Kind,
    ENDMARKER, # EOF
    COMMENT, # aadsdsa, #= fdsf #=
    WHITESPACE, # '   \t'
    NEWLINE, # \n
    ESCD_NEWLINE, # \ \n
    IDENTIFIER, # foo, Σxx
    ESCD_IDENTIFIER, # \***myfancyidentifier***
    SYSTEM_IDENTIFIER, # $id
    BASE_SPEC, # 'b (but not hex)
    HEX_BASE_SPEC, # 'h or 'H
    # Any identifier on the reserved list that does not have its
    # own token.
    RESERVED,
    BACKTICK, # `
    BACKTICK_BACKTICK, # ``
    BACKTICK_QUOTE, # `"
    M_DEFINE, # define (after BACKTICK)
    M_UNDEF, # undef (after BACKTICK)
    M_IFDEF, # ifdef (after BACKTICK)
    M_IFNDEF, # ifndef (after BACKTICK)
    M_ELSE, # else (after BACKTICK)
    M_ELSIF, # elsif (after BACKTICK)
    M_ENDIF, # endif (after BACKTICK)
    COMMA, #,
    COLON, #:
    SEMICOLON, # ;
    EQ, # =
    DOT, # .
    AT_SIGN, # @
    HASH, # #
    DOLLAR, # $
    CONDITIONAL, # ?

    # We lex `include as BACKTICK M_INCLUDE TRIVIA M_INCLUDE_FNAME TRIVIA NEWLINE
    M_INCLUDE, # include (after BACKTICK)
    M_INCLUDE_TRIVIA_QUOTE,
    M_INCLUDE_TRIVIA_ANGLE,
    M_INCLUDE_TRIVIA_QUOTE_CLOSE,
    M_INCLUDE_TRIVIA_ANGLE_CLOSE,
    M_INCLUDE_FNAME,

    begin_errors,
        ERROR, # Generic Error

        # The following are preprocessor errors not emitted by the lexer, but since
        # the preprocessor spits out tokens, we reserve these here for the
        # preprocessor to synthesize.

        # These tokens are used in place of the BACKTICK that would have otherwise
        # been there.
        PREPROC_ERR_MISSING_MACRO,
        PREPROC_ERR_UNDEF_MACRO,
        PREPROC_ERR_RECURSIVE_MACRO,
        PREPROC_ERR_EXPECTED_LPAREN,

        # For incomplete macro calls, we emit tokens as:
        # PREPROC_ERR_BAD_MACRO_CALL LAPREN PREPROC_ERR_BAD_MACRO_PARAM COMMA
        #   ... COMMA PREPROC_ERR_BAD_MACRO_PARAM RPAREN

        PREPROC_ERR_BAD_MACRO_CALL,
        PREPROC_ERR_BAD_MACRO_PARAM,

        EOF_MULTICOMMENT,
        EOF_STRING,
        INVALID_BASE_SPEC,
        INCLUDE_DIRECTIVE_JUNK,
        UNKNOWN,
    end_errors,

    begin_keywords,
        MODULE, # module
        MACROMODULE, # macromodule
        CONNECTMODULE, # connectmodule
        ENDMODULE, # endmodule
        PRIMITIVE, # primitive
        ENDPRIMITIVE, # endprimitive
        CONFIG, # config
        ENDCONFIG, # endconfig
        PARAMSET, # paramset
        ENDPARAMSET, # endparamset
        DISCIPLINE, # discipline
        ENDDISCIPLINE, # enddiscipline
        NATURE, # nature
        ENDNATURE, # endnature
        CONNECTRULES, # connectrules
        ENDCONNECTRULES, # endconnectrules
        CASE, # case
        CASEX, # casex
        CASEZ, # casez
        ENDCASE, # endcase
        INOUT, # inout
        INPUT, # input
        OUTPUT, # output
        ALIASPARAM, # aliasparam
        PARAMETER, # parameter
        REAL, # real
        ANALOG, # analog
        INITIAL, # initial
        REG, # reg
        WREAL, # wreal
        SIGNED, # signed
        INTEGER, # integer
        REALTIME, # realtime
        TIME, # time
        STRING, # string
        FROM, # from
        EXCLUDE, # exclude
        BEGIN, # begin
        END, # end
        FLOW, # flow
        POTENTIAL, # potential
        DOMAIN, # domain
        DISCRETE, # discrete
        CONTINUOUS, # continuous
        ABSTOL, # abstol
        ACCESS, # access
        DDT_NATURE, # ddt_nature
        IDT_NATURE, # idt_nature
        UNITS, # units
        FUNCTION, # function
        ENDFUNCTION, # endfunction
        INF, # inf
        BRANCH, # branch

        IF, # if
        ELSE, # else
        FOR, # for
        WHILE, # while
        REPEAT, # repeat
        DEFAULT, # default

        begin_net_types,
            SUPPLY0, # supply0
            SUPPLY1, # supply1
            TRI, # tri
            TRIAND, # triand
            TRIOR, # trior
            TRI0, # tri0
            TRI1, # tri1
            UWIRE, # uwire
            WIRE, # wire
            WAND, # wand
            WOR, # wor
        end_net_types,
    end_keywords,

    begin_cstparser,
    end_cstparser,

    begin_literal,
        LITERAL, # general
        INT, # 4
        # N.B.: It is possible for hex constants to get mis-lexed as identifier,
        #       since base constants can be substituted through macro expansion.
        #       This ambiguity needs to be resolved in parsing.
        HEX_INT, # 0fff0
        FLOAT, # 3.5, 3.7e+3
        STR, # "foo"
    end_literal,

    begin_delimiters,
        LSQUARE, # [
        RSQUARE, # [
        LBRACE, # {
        AP_LBRACE, # '{
        RBRACE, # }
        LPAREN, # (
        RPAREN,  # )
        LATTR, # (*
        RATTR, # *)
    end_delimiters,

    begin_filter_functions,
        DDT, # ddt
        DDX, # ddx
        IDT, # idt
        IDTMOD, # idtmod
        ABDSDELAY, # absdelay
        TRANSISTION, # transition
        SLEW, # slew
        LAST_CROSSING, # last_crossing
        LIMEXP, # limexp
    end_filter_functions,

    begin_small_signal_functions,
        WHITE_NOISE, # white_noise
        FLICKER_NOISE, # flicker_noise
        NOISE_TABLE, # noise_table
        NOISE_TABLE_LOG , # noise_table_log
    end_small_signal_functions,

    begin_ops,
        begin_unary_ops,
            # Logical negation
            NOT, # !

            # Bitwise
            TILDE, # ~

            # Reduction
            TILDE_OR,  # ~|
            TILDE_AND, # ~&

        begin_binary_ops,
            OP, # general

            # Arithmenic
            PLUS, # +
            MINUS, # -

            # Bitwise
            AND, # &
            OR, # |
            XOR, # ^

            # Reduction
            XOR_TILDE, # ^~
            TILDE_XOR, # ~^

        end_unary_ops,
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


            # Logical and/or
            LAZY_AND, # &&
            LAZY_OR, # ||

            # Bitshifts
            LBITSHIFT, # <<
            RBITSHIFT, # >>
            LBITSHIFT_A, # <<<
            RBITSHIFT_A, # >>>

            # Literal "or"
            EVENT_OR, # or
        end_binary_ops,
    end_ops,

    # Arithmetic assignment operators (SystemVerilog Extension)
    PLUS_EQ, # +=
    MINUS_EQ, # -=
    STAR_EQ, # *=
    SLASH_EQ, # /=
    STAR_STAR_EQ, # **=
    PERCENT_EQ, # %=
    PERCENT_PERCENT, # %%
    AND_EQ, # &=
    OR_EQ, # |=
    XOR_EQ, # ^=
    LBITSHIFT_EQ, # <<=
    RBITSHIFT_EQ, # >>=
    LBITSHIFT_A_EQ, # <<<=
    RBITSHIFT_A_EQ, # >>>=

    # Continus assignment
    CASSIGN, # <+
)

end
using .Tokens

isident(k::Kind) = k == IDENTIFIER || k == ESCD_IDENTIFIER
Lexers.is_triv(k::Kind) = k == COMMENT || k == WHITESPACE || k == NEWLINE
Lexers.is_newline(k::Kind) = k == NEWLINE
is_builtin_func(k::Kind) = begin_builtin_functions < k < end_builtin_functions
is_filter_func(k::Kind) = begin_filter_functions < k < end_filter_functions
is_small_sig_func(k::Kind) = begin_small_signal_functions < k < end_small_signal_functions
iskeyword(k::Kind) = begin_keywords < k < end_keywords
isliteral(k::Kind) = begin_literal < k < end_literal
isoperator(k::Kind) = begin_ops < k < end_ops
is_binary_operator(k::Kind) = begin_binary_ops < k < end_binary_ops
is_unary_operator(k::Kind) = begin_unary_ops < k < end_unary_ops

is_parameter_type(k::Kind) = k in (INTEGER, REAL, REALTIME, TIME, STRING)

# Create string => keyword kind
const KEYWORDS = Dict{String, Kind}()

function _add_kws()
    for k in instances(Kind)
        if iskeyword(k)
            KEYWORDS[lowercase(string(k))] = k
        end
    end
end
_add_kws()
