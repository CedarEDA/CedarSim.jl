module SPICENetlistCSTParser

using ..SPICENetlistTokenize
using ..SPICENetlistTokenize: Lexer, Token, next_token, kind,
    is_kw, is_operator, is_unary_operator, is_literal, is_ident,
    is_analysis, is_number, is_source_type
    # is_builtin_func, is_builtin_const
using ..SPICENetlistTokenize.Tokens
import ..SPICENetlistTokenize: peekchar
using ..SPICENetlistParser

using ...LineNumbers
using ...EXPRS
using ...RedTree

include("forms.jl")
include("parserstate.jl")
include("parse.jl")

function parse(str::AbstractString; offset=0, kwargs...)
    io = IOBuffer(str)
    seek(io, offset)
    return parse(io; kwargs...)
end
parsefile(fname::AbstractString, kwargs...) = parse(String(open(read, fname)); fname, kwargs...)

function parse(io::IOBuffer; fname=nothing, return_on_language_change::Bool=false)
    return parse(ParseState(io; fname, return_on_language_change))
end

function parse(ps::ParseState)
    stmts = EXPRList{Any}()
    while kind(nt(ps)) !== ENDMARKER
        push!(stmts, parse_spice_toplevel(ps))
        if ps.return_on_language_change && ps.lang_swapped
            break
        end
    end

    return Node(ps, ps.startpos, nothing, EXPR(SPICENetlistSource(stmts)))
end

end # module
