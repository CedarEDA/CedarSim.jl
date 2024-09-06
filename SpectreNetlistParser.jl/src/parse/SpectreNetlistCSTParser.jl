module SpectreNetlistCSTParser

using ..SpectreNetlistTokenize
using ..SpectreNetlistTokenize: Lexer, Token, next_token, kind,
    is_kw, is_operator, is_unary_operator, is_literal, is_ident, is_analysis, is_number, is_save_kw,
    is_builtin_func, is_builtin_const
using ..SpectreNetlistTokenize.Tokens

import ..SPICENetlistParser

using ..LineNumbers
using ..EXPRS
using ..RedTree

include("forms.jl")
include("parserstate.jl")
include("parse.jl")

function parse(str::AbstractString; offset=0, kwargs...)
    io = IOBuffer(str)
    seek(io, offset)
    return parse(io; kwargs...)
end
parsefile(fname::AbstractString; kwargs...) = parse(String(open(read, fname)); fname, kwargs...)

function parse(io::IOBuffer; fname=nothing, start_lang=nothing, enable_julia_escape::Bool=false, implicit_title::Bool=true, line_offset::Int=0)
    if start_lang === nothing
        start_lang = if fname === nothing
            :spectre
        else
            _, ext = splitext(fname)
            if ext == ".scs"
                :spectre
            else
                :spice
            end
        end
    end

    ps = ParseState(io; fname, line_offset)
    parse(ps; start_lang, enable_julia_escape, implicit_title)
end

function transition_from_spice!(ps::ParseState, ps_spice)
    p = position(ps_spice)
    seek(ps.srcfile.contents, p)
    reinit_at_pos!(ps, p)
end

function parse(ps::ParseState; start_lang, enable_julia_escape::Bool=false, implicit_title::Bool=false)
    stmts = EXPRList{Any}()

    if start_lang == :spice
        seek(ps.srcfile.contents, 0)
        ps_spice = SPICENetlistParser.SPICENetlistCSTParser.ParseState(ps.srcfile; return_on_language_change=true, enable_julia_escape, implicit_title, line_offset=ps.line_offset)
        tree_spice = SPICENetlistParser.SPICENetlistCSTParser.parse(ps_spice)
        push!(stmts, tree_spice.expr)
        transition_from_spice!(ps, ps_spice)
    end

    while kind(nt(ps)) !== ENDMARKER
        push!(stmts, parse_spectrenetlist_source(ps))
        if ps.lang_swapped
            offset = nt(ps).startbyte
            seek(ps.srcfile.contents, offset)
            ps_spice = SPICENetlistParser.SPICENetlistCSTParser.ParseState(ps.srcfile; return_on_language_change=true)
            tree_spice = SPICENetlistParser.SPICENetlistCSTParser.parse(ps_spice)
            push!(stmts, tree_spice.expr)
            transition_from_spice!(ps, ps_spice)
        end
    end

    return Node(ps, ps.startpos, nothing, EXPR(SpectreNetlistSource(stmts)))
end

end # module
