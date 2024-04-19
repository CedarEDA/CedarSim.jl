module VerilogACSTParser
    using ..VerilogATokenize
    using ..VerilogATokenize: Lexer, Token, next_token, kind,
        iskeyword, is_parameter_type, isoperator, isliteral, isident,
        is_filter_func, is_small_sig_func, is_unary_operator, is_binary_operator
    using ..VerilogATokenize.Tokens
    import ..VerilogATokenize: peekchar

    include("LineNumbers.jl")
    using .LineNumbers

    include("expr.jl")
    include("utils.jl")
    include("preproc.jl")
    include("forms.jl")
    include("parse.jl")
    include("expanded.jl")
    include("errors.jl")

    function parse(str::Union{String, IOBuffer}, cont=false)
        ps = ParseState(str)
        parse(ps, cont)
    end

    function parsefile(str::String, cont=false)
        ps = ParseState(IOBuffer(open(read, str)), str)
        parse(ps, cont)
    end

    function parse(ps::ParseState, cont=false)
        stmts = EXPRList{Any}()
        while kind(nt(ps)) !== ENDMARKER
            push!(stmts, parse_verilog_source(ps))
        end

        return Node(ps, VirtPos(UInt32(0)), nothing, EXPR(VerilogSource(stmts)))
    end

end
