
import ..@case

function parse_spectrenetlist_source(ps)::EXPR
    ex = @case kind(nt(ps)) begin
        SIMULATOR => parse_simulator(ps)
        MODEL => parse_model(ps)
        INCLUDE => parse_include(ps)
        AHDL_INCLUDE => parse_ahdl_include(ps)
        GLOBAL => parse_global(ps)

        PARAMETERS => parse_parameters(ps)

        INLINE => parse_subckt(ps, take_kw(ps, INLINE))
        SUBCKT => parse_subckt(ps)
        # Control
        SAVE => parse_save(ps)
        IC => parse_ic(ps)
        NODESET => parse_nodeset(ps)
        #
        REAL => parse_function_decl(ps)
        #
        IF => parse_conditional_block(ps)

        # Error
        NEWLINE => error("internal error: forgot to eat a newline?")
    end
    ex === nothing || return ex
    if is_ident(kind(nt(ps)))
        return parse_other(ps)
    end
    return error!(ps, UnexpectedToken)
end

function parse_conditional_block(ps)
    aif = parse_if(ps)
    cases = EXPRList{ElseIf}()
    aelse = nothing
    while kind(nt(ps)) == ELSE
        kw = take_kw(ps, ELSE)
        if kind(nt(ps)) == IF
            stmt = parse_elseif(ps, kw)
            push!(cases, stmt)
        else
            aelse = parse_else(ps, kw)
            break
        end
    end
    nl = accept_newline(ps)
    return EXPR(ConditionalBlock(aif, cases, aelse, nl))
end

function parse_if(ps)
    kw = take_kw(ps, IF)
    lparen = accept(ps, LPAREN)
    expr = parse_expression(ps)
    rparen = accept(ps, RPAREN)
    lbrace = accept(ps, LBRACE)
    nl = accept_newline(ps)
    name = take_identifier(ps)
    stmt = parse_instance(ps,name)
    rbrace = accept(ps, RBRACE)
    return EXPR(If(kw, lparen, expr, rparen, lbrace, nl, stmt, rbrace))
end

function parse_elseif(ps, kw)
    kw2 = take_kw(ps, IF)
    lparen = accept(ps, LPAREN)
    expr = parse_expression(ps)
    rparen = accept(ps, RPAREN)
    lbrace = accept(ps, LBRACE)
    nl = accept_newline(ps)
    name = take_identifier(ps)
    stmt = parse_instance(ps,name)
    rbrace = accept(ps, RBRACE)
    return EXPR(ElseIf(kw, kw2, lparen, expr, rparen, lbrace, nl, stmt, rbrace))
end

function parse_else(ps, kw)
    lbrace = accept(ps, LBRACE)
    nl = accept_newline(ps)
    name = take_identifier(ps)
    stmt = parse_instance(ps,name)
    rbrace = accept(ps, RBRACE)
    return EXPR(Else(kw, lbrace, nl, stmt, rbrace))
end



function parse_function_decl_arg(ps)
    typ = take_kw(ps, REAL)
    id = take_identifier(ps)
    return EXPR(FunctionDeclArg(typ, id))
end

function parse_function_decl(ps)
    rtype = take_kw(ps, REAL)
    id = take_identifier(ps)
    lparen = take(ps, LPAREN)
    args = EXPRList{FunctionArgs{EXPR{FunctionDeclArg}}}()
    if kind(nt(ps)) != RPAREN
        parse_comma_list!(parse_function_decl_arg, ps, args)
    end
    rparen = take(ps, RPAREN)
    lbrace = take(ps, LBRACE)
    nl1 = take(ps, NEWLINE)
    ret = take_kw(ps, RETURN)
    exp = parse_expression(ps)
    semicolon = take(ps, SEMICOLON)
    nl2 = take(ps, NEWLINE)
    rbrace = take(ps, RBRACE)
    nl3 = take(ps, NEWLINE)
    return EXPR(FunctionDecl(rtype, id, lparen, args, rparen, lbrace, nl1, ret, exp, semicolon, nl2, rbrace, nl3))
end

function parse_subckt(ps, inline=nothing)
    kw = accept_kw(ps, SUBCKT)
    name = accept_identifier(ps)
    subckt_nodes = if kind(nt(ps)) == LPAREN
        lparen = accept(ps, LPAREN)
        nodes = parse_nodes(ps)
        rparen = accept(ps, RPAREN)
        EXPR(SubcktNodes(lparen, nodes, rparen))
    else
        nodes = parse_nodes(ps)
        if isempty(nodes)
            nothing
        else
            EXPR(SubcktNodes(nothing, nodes, nothing))
        end
    end
    nl = accept_newline(ps)
    exprs = EXPRList{Any}()
    while kind(nt(ps)) != ENDS
        expr = parse_spectrenetlist_source(ps)
        push!(exprs, expr)
    end
    ends = accept_kw(ps, ENDS)
    end_name = nothing
    if kind(nt(ps)) == IDENTIFIER
        end_name = take_identifier(ps)
    end
    nl2 = accept_newline(ps)
    return EXPR(Subckt(inline, kw, name, subckt_nodes, nl, exprs, ends, end_name, nl2))
end

function parse_paramtest(ps, name)
    kw = accept_kw(ps, PARAMTEST)
    params = parse_parameter_list(ps)
    nl = accept_newline(ps)
    return EXPR(ParamTest(name, kw, params, nl))
end

function parse_node(ps)
    subckts = EXPRList{SubcktNode}()
    id = take_node(ps)
    while kind(nt(ps)) == DOT
        dot = take(ps, DOT)
        push!(subckts, EXPR(SubcktNode(id, dot)))
        id = take_node(ps)
    end
    return EXPR(SNode(subckts, id))
end

function take_node(ps)
    if kind(nt(ps)) == INT_LIT
        return take_literal(ps)
    elseif is_ident(kind(nt(ps)))
        return take_identifier(ps)
    else
        error!(ps, UnexpectedToken)
    end
end

function parse_global(ps)
    kw = take_kw(ps, GLOBAL)
    nodes = EXPRList{SNode}()
    while !eol(ps)
        push!(nodes, parse_node(ps))
    end
    nl = accept_newline(ps)
    return EXPR(Global(kw, nodes, nl))
end

function parse_other(ps)
    name = take_identifier(ps)
    if is_analysis(kind(nt(ps)))
        return parse_analysis(ps, name)
    end
    @case kind(nt(ps)) begin
        LPAREN => parse_instance(ps, name) # TODO: paren is optional...
        ALTERGROUP => parse_altergroup(ps, name)
        ALTER => parse_alter(ps, name)
        CHECK => parse_check(ps, name)
        CHECKLIMIT => parse_checklimit(ps, name)
        INFO => parse_info(ps, name)
        OPTIONS => parse_options(ps, name)
        SET => parse_set(ps, name)
        SHELL => parse_shell(ps, name)
        PARAMTEST => parse_paramtest(ps, name)
        _ => error!(ps, UnexpectedToken)
    end
end

function parse_save_signal(ps)
    signalname = nothing
    if kind(nt(ps)) != COLON
        signalname = parse_node(ps)
    end
    modifier = nothing
    if kind(nt(ps)) == COLON
        col = take(ps, COLON)
        if is_save_kw(kind(nt(ps)))
            mod = take_kw(ps)
        elseif is_number(kind(nt(ps)))
            mod = take_literal(ps)
        elseif is_ident(kind(nt(ps)))
            mod = take_identifier(ps)
        else
            error!(ps, UnexpectedToken)
        end
        modifier = EXPR(SaveSignalModifier(col, mod))
    end
    return EXPR(SaveSignal(signalname, modifier))
end

function parse_save_list(ps)
    signals = EXPRList{SaveSignal}() # TODO
    while !eol(ps)
        signal = parse_save_signal(ps)
        push!(signals, signal)
    end
    return signals
end

function parse_save(ps)
    kw = take_kw(ps)
    signals = parse_save_list(ps)
    nl = accept_newline(ps)
    return EXPR(Save(kw, signals, nl))
end


function parse_ic_parameter_list(ps)
    parameters = EXPRList{ICParameter}()
    while !eol(ps)
        p = parse_ic_parameter(ps)
        push!(parameters, p)
    end
    return parameters
end

function parse_ic_parameter(ps)
    name = parse_node(ps)
    eq = accept(ps, EQ)
    val = parse_expression(ps)
    return EXPR(ICParameter(name, eq, val))
end

function parse_ic(ps)
    kw = take_kw(ps, IC)
    parameters = parse_ic_parameter_list(ps)
    nl = take(ps, NEWLINE)
    return EXPR(Ic(kw, parameters, nl))
end

function parse_nodeset(ps)
    kw = take_kw(ps, NODESET)
    parameters = parse_parameter_list(ps)
    nl = take(ps, NEWLINE)
    return EXPR(NodeSet(kw, parameters, nl))
end



function parse_altergroup(ps, name)
    kw = accept_kw(ps, ALTERGROUP)
    lbrace = accept(ps, LBRACE)
    nl1 = accept_newline(ps)
    exprs = EXPRList{Any}()
    while kind(nt(ps)) != RBRACE
        push!(exprs, parse_spectrenetlist_source(ps))
    end
    rbrace = accept(ps, RBRACE)
    nl2 = accept_newline(ps)
    return EXPR(AlterGroup(name, kw, lbrace, nl1, exprs, rbrace, nl2))
end

function parse_alter(ps, name)
    kw = accept_kw(ps, ALTER)
    params = parse_parameter_list(ps)
    nl = accept_newline(ps)
    return EXPR(Alter(name, kw, params, nl))
end


function parse_check(ps, name)
    kw = accept_kw(ps, CHECK)
    params = parse_parameter_list(ps)
    nl = accept_newline(ps)
    return EXPR(Check(name, kw, params, nl))
end

function parse_checklimit(ps, name)
    kw = accept_kw(ps, CHECKLIMIT)
    params = parse_parameter_list(ps)
    nl = accept_newline(ps)
    return EXPR(CheckLimit(name, kw, params, nl))
end

function parse_info(ps, name)
    kw = accept_kw(ps, INFO)
    params = parse_parameter_list(ps)
    nl = accept_newline(ps)
    return EXPR(Info(name, kw, params, nl))
end

function parse_options(ps, name)
    kw = accept_kw(ps, OPTIONS)
    params = parse_parameter_list(ps)
    nl = accept_newline(ps)
    return EXPR(Options(name, kw, params, nl))
end

function parse_set(ps, name)
    kw = accept_kw(ps, SET)
    params = parse_parameter_list(ps)
    nl = accept_newline(ps)
    return EXPR(Set(name, kw, params, nl))
end

function parse_shell(ps, name)
    kw = accept_kw(ps, SHELL)
    params = parse_parameter_list(ps)
    nl = accept_newline(ps)
    return EXPR(Shell(name, kw, params, nl))
end



function parse_analysis(ps, name, nodelist=nothing)
    kw = accept_kw(ps)
    params = parse_parameter_list(ps)
    nl = accept_newline(ps)
    return EXPR(Analysis(name, nodelist, kw, params, nl))
end

function parse_instance(ps, name)
    lparen = accept(ps, LPAREN) # TODO: parenthesis are optional
    nodes = parse_nodes(ps)
    rparen = accept(ps, RPAREN)
    nodelist = EXPR(SNodeList(lparen, nodes, rparen))
    if is_analysis(kind(nt(ps)))
        return parse_analysis(ps, name, nodelist)
    end
    master = take_identifier(ps)
    params = parse_parameter_list(ps)
    nl = accept_newline(ps)
    return EXPR(Instance(name, nodelist, master, params, nl))
end

function parse_nodes(ps)
    nodes = EXPRList{SNode}()
    while kind(nt(ps)) != RPAREN && !eol(ps)
        id = parse_node(ps)
        push!(nodes, id)
    end
    return nodes
end

function parse_parameter_list(ps)
    parameters = EXPRList{Parameter}()
    while !eol(ps)
        p = parse_parameter(ps)
        push!(parameters, p)
    end
    return parameters
end

eol(ps) = (t = nt(ps); kind(t) == NEWLINE || kind(t) == ENDMARKER)

function parse_parameter(ps)
    name = take_identifier(ps)
    eq = accept(ps, EQ)
    val = parse_expression(ps)
    return EXPR(Parameter(name, eq, val))
end

function parse_parameters(ps)
    kw = accept_kw(ps, PARAMETERS)
    params = parse_parameter_list(ps)
    nl = accept_newline(ps)
    return EXPR(Parameters(kw, params, nl))
end

function parse_expression(ps)
    ex = parse_primary_or_unary(ps)
    if is_operator(kind(nt(ps)))
        op = take_operator(ps)
        ex = parse_binop(ps, ex, op)
    elseif kind(nt(ps)) == CONDITIONAL
        not = take(ps, CONDITIONAL)
        ifcase = parse_expression(ps)
        colon = accept(ps, COLON)
        elsecase = parse_expression(ps)
        ex = EXPR(TernaryExpr(ex, not, ifcase, colon, elsecase))
    end
    return ex
end


function parse_comma_list!(parse_item, ps, list)
    comma = nothing
    while true
        ref = parse_item(ps)
        push!(list, EXPR((typeof(list).parameters[1])(comma, ref)))
        kind(nt(ps)) == COMMA || return
        comma = take(ps, COMMA)
    end
    nothing
end


function parse_function_call(ps, name)
    lparen = accept(ps, LPAREN)
    args = EXPRList{FunctionArgs{EXPR}}()
    if kind(nt(ps)) != RPAREN
        parse_comma_list!(parse_expression, ps, args)
    end
    return EXPR(FunctionCall(name, lparen, args, accept(ps, RPAREN)))
end

function parse_primary_or_unary(ps)
    if is_unary_operator(kind(nt(ps)))
        parse_unary_op(ps)
    else
        parse_primary(ps)
    end
end

function parse_unary_op(ps)
    op = take_operator(ps)
    primary = parse_primary(ps)
    return EXPR(UnaryOp(op, primary))
end


function parse_binop(ps, ex, op, opterm = nothing)
    local rhs
    while true
        rhs = parse_primary_or_unary(ps)
        is_operator(kind(nt(ps))) || break
        ntprec = prec(kind(nt(ps)))
        if prec(op) >= ntprec
            ex = EXPR(BinaryExpression(ex, op, rhs))
            (opterm !== nothing && prec(opterm) >= ntprec) && return ex
            op = take_operator(ps)
            continue
        else
            rhs = parse_binop(ps, rhs, take_operator(ps), op)
            ex = EXPR(BinaryExpression(ex, op, rhs))
            is_operator(kind(nt(ps))) || return ex
            op = take_operator(ps)
            continue
        end
    end
    ret = EXPR(BinaryExpression(ex, op, rhs))
    return ret
end


function parse_primary(ps)
    if is_number(kind(nt(ps)))
        lit = take_literal(ps)
        unit = nothing
        if kind(nt(ps)) == UNIT
            unit = take_literal(ps)
        end
        return EXPR(NumericValue(lit, unit))
    elseif is_literal(kind(nt(ps)))
        return take_literal(ps)
    elseif is_builtin_const(kind(nt(ps)))
        return take_builtin_const(ps)
    elseif is_ident(kind(nt(ps))) || is_builtin_func(kind(nt(ps)))
        if is_builtin_func(kind(nt(ps)))
            id = take_builtin_func(ps)
        else
            id = take_identifier(ps)
        end
        if kind(nt(ps)) == LPAREN
            return parse_function_call(ps, id)
        else
            return id
        end
    elseif kind(nt(ps)) == STRING
        return take_string(ps)
    elseif kind(nt(ps)) == LSQUARE
        return parse_array(ps)
    elseif is_kw(kind(nt(ps)))
        return take_kw(ps)
    elseif kind(nt(ps)) == LPAREN
        return parse_paren(ps)
    end
    if kind(nt(ps)) in (RPAREN, RBRACE, RSQUARE, COMMA, SEMICOLON, BACKTICK, EQ, LATTR, RATTR, ERROR, COLON, ENDMARKER) || is_kw(kind(nt(ps))) || is_operator(kind(nt(ps)))
        return error!(ps, UnexpectedToken)
    end
    error("internal error: unreachable $(kind(nt(ps)))")
end

function parse_paren(ps)
    lparen = take(ps, LPAREN)
    e = parse_expression(ps)
    rparen = accept(ps, RPAREN)
    return EXPR(Parens(lparen, e, rparen))
end


function parse_array(ps)
    lbrace = take(ps, LSQUARE)
    items = EXPRList{Any}()
    while kind(nt(ps)) !== RSQUARE
        # > When expressions are used within vectors, anything other than constants, parameters,
        # or unary expressions (unary +, unary -) must be surrounded by parentheses
        if kind(nt(ps)) == LPAREN
            v = parse_paren(ps)
        elseif is_unary_operator(kind(nt(ps)))
            v = parse_unary_op(ps)
        elseif is_ident(kind(nt(ps)))
            v = take_identifier(ps)
        elseif is_literal(kind(nt(ps)))
            v = take_literal(ps)
        else
            return error!(ps, UnexpectedToken)
        end
        push!(items, v)
    end
    rbrace = take(ps, RSQUARE)
    return EXPR(SpectreArray(lbrace, items, rbrace))
end

function parse_include(ps)
    kw = take_kw(ps, INCLUDE)
    str = take_string(ps)
    section = nothing
    if kind(nt(ps)) == SECTION
        section = parse_include_section(ps)
    end
    nl = accept_newline(ps)
    return EXPR(Include(kw, str, section, nl))
end

function parse_ahdl_include(ps)
    kw = take_kw(ps, AHDL_INCLUDE)
    str = take_string(ps)
    nl = accept_newline(ps)
    return EXPR(AHDLInclude(kw, str, nl))
end

function parse_include_section(ps)
    kw_sec = take_kw(ps, SECTION)
    eq = take(ps, EQ)
    sec = take_identifier(ps)
    return EXPR(IncludeSection(kw_sec, eq, sec))
end



function parse_simulator(ps)
    kw = take_kw(ps, SIMULATOR)
    langkw = take_kw(ps, LANG)
    eq = take(ps, EQ)
    if kind(nt(ps)) == SPICE
        ps.lang_swapped=true
    end
    lang = take_kw(ps, (SPECTRE, SPICE))
    params = parse_parameter_list(ps)
    nl = accept_newline(ps)
    return EXPR(Simulator(kw, langkw, eq, lang, params, nl))
end


function parse_model(ps)
    kw = take_kw(ps, MODEL)
    name = take_identifier(ps)
    master = take_identifier(ps)
    parameters = parse_parameter_list(ps)
    nl = accept_newline(ps)
    return EXPR(Model(kw, name, master, parameters, nl))
end

function take_kw(ps)
    kwkind = kind(nt(ps))
    @assert is_kw(kwkind)
    EXPR!(Keyword(kwkind), ps)
end

function take_kw(ps, tkind)
    !isa(tkind, Tuple) && (tkind = (tkind,))
    kwkind = kind(nt(ps))
    @assert is_kw(kwkind) && kwkind in tkind
    EXPR!(Keyword(kwkind), ps)
end

function take_identifier(ps)
    if !(is_ident(kind(nt(ps))))
        error!(ps, UnexpectedToken)
    end
    return EXPR!(Identifier(), ps)
end



function take_literal(ps)
    ntkind = kind(nt(ps))
    @assert is_literal(ntkind)
    EXPR!(ntkind == FLOAT ? FloatLiteral() :
          ntkind == INT_LIT ? IntLiteral()   :
          ntkind == UNIT  ? UnitLiteral()  :
          Literal(), ps)
end

function accept_identifier(ps)
    if kind(nt(ps)) == IDENTIFIER
        return take_identifier(ps)
    else
        return error!(ps, UnexpectedToken)
    end
end

function accept(ps, tkind)
    !isa(tkind, Tuple) && (tkind = (tkind,))
    if kind(nt(ps)) in tkind
        return EXPR!(Notation(), ps)
    else
        return error!(ps, UnexpectedToken)
    end
end

function accept_newline(ps)
    if kind(nt(ps)) in (NEWLINE, ENDMARKER)
        return EXPR!(Notation(), ps)
    else
        return error!(ps, UnexpectedToken)
    end
end

function accept_kw(ps, tkind)
    !isa(tkind, Tuple) && (tkind = (tkind,))
    @assert all(is_kw, tkind)
    kwkind = kind(nt(ps))
    if kwkind in tkind
        return EXPR!(Keyword(kwkind), ps)
    else
        return error!(ps, UnexpectedToken)
    end
end

function accept_kw(ps)
    kwkind = kind(nt(ps))
    if is_kw(kwkind)
        return EXPR!(Keyword(kwkind), ps)
    else
        return error!(ps, UnexpectedToken)
    end
end

function take_string(ps)
    @assert kind(nt(ps)) == STRING
    return EXPR!(StringLiteral(), ps)
end

function take(ps, tkind)
    !isa(tkind, Tuple) && (tkind = (tkind,))
    @assert kind(nt(ps)) in tkind
    return EXPR!(Notation(), ps)
end

function take_operator(ps)
    ntkind = kind(nt(ps))
    @assert is_operator(ntkind)
    return EXPR!(Operator(ntkind), ps)
end

function take_builtin_func(ps)
    @assert is_builtin_func(kind(nt(ps)))
    return EXPR!(BuiltinFunc(), ps)
end

function take_builtin_const(ps)
    @assert is_builtin_const(kind(nt(ps)))
    return EXPR!(BuiltinConst(), ps)
end

function error!(ps, kind, expected=nothing)
    # TODO Make the errors emit error tokens and show all of the errors as in VerilogParser
    ps.errored = true
    # debug && error("error token with kind $(nt(ps))")
    throw(SpectreParserError(ps, kind, expected))
end

struct SpectreParserError <: Exception
    ps
    kind
    expected
end

function Base.showerror(io::IO, s::SpectreParserError)
    ps = s.ps
    nt = ps.nt
    lb = LineBreaking(UInt64(0), ps.srcfile.lineinfo, nothing)
    line, col1 = LineNumbers.indtransform(lb, nt.startbyte)
    _, col2 = LineNumbers.indtransform(lb, nt.endbyte)
    line_str = String(ps.srcfile.lineinfo[line])
    print(io, "SpectreParser error at line $(line):")
    if s.expected !== nothing
        print(io, "expected $(s.expected)")
    end
    println(io)
    println(io, "  ", chomp(line_str))
    print(io, "  ")
    for i in 1:length(line_str)
        if i >= col1 && i <= col2
            printstyled(io, "^"; color=:light_green)
        else
            print(io, " ")
        end
    end
end

@enum(PrecedenceLevels,
    PREC_LOGICAL,
    PREC_AND_AND,
    PREC_OR,
    PREC_XOR,
    PREC_AND,
    PREC_EQ,
    PREC_LT,
    PREC_SHIFT,
    PREC_PLUS,
    PREC_MUL,
    PREC_STAR_STAR)

function prec(opkind::Kind)
    if opkind in (LAZY_OR, EVENT_OR)
        return PREC_LOGICAL
    elseif opkind in (LAZY_AND,)
        return PREC_AND_AND
    elseif opkind in (OR,)
        return PREC_OR
    elseif opkind in (XOR, XOR_TILDE, TILDE_XOR)
        return PREC_XOR
    elseif opkind in (AND,)
        return PREC_AND
    elseif opkind in (EQEQ, NOT_EQ, EQEQEQ, NOT_EQEQ)
        return PREC_EQ
    elseif opkind in (LESS, GREATER, LESS_EQ, GREATER_EQ)
        return PREC_LT
    elseif opkind in (LBITSHIFT, RBITSHIFT, LBITSHIFT_A, RBITSHIFT_A)
        return PREC_SHIFT
    elseif opkind in (PLUS, MINUS)
        return PREC_PLUS
    elseif opkind in (STAR, SLASH, PERCENT)
        return PREC_MUL
    elseif opkind in (STAR_STAR,)
        return PREC_STAR_STAR
    else
        error("Unknown operator")
    end
end
prec(op::Operator) = prec(op.op)
prec(ex::EXPR{Operator}) = prec(ex.form)
