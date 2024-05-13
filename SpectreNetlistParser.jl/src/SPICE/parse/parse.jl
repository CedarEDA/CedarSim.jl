# TODO:
# - Move endl inline
# - Nodes can be integers

import ...@case

function parse_spice_toplevel(ps)::EXPR
    ex = @case kind(nt(ps)) begin
        SIMULATOR => parse_simulator(ps)
        DOT => parse_dot(ps)
        TITLE_LINE => parse_title(ps)
        NEWLINE => error("internal error: forgot to eat a newline?")
    end
    ex === nothing || return ex
    if is_ident(kind(nt(ps)))
        return parse_instance(ps)
    end
    error!(ps, UnexpectedToken)
end


function parse_dot(ps)
    dot = take(ps, DOT)
    parse_dot(ps, dot)
end

function parse_dot(ps, dot)
    @case kind(nt(ps)) begin
        DC => parse_dc(ps, dot)
        AC => parse_ac(ps, dot)
        LIB => parse_lib(ps, dot)
        END => parse_end(ps, dot)
        ENDL => parse_endl(ps, dot) # Only when parsing a library?
        INCLUDE => parse_include(ps, dot)
        PARAMETERS => parse_param(ps, dot)
        CSPARAM => parse_csparam(ps, dot)
        SUBCKT => parse_subckt(ps, dot)
        MODEL => parse_model(ps, dot)
        OPTIONS => parse_options(ps, dot)
        TITLE => parse_title(ps, dot)
        GLOBAL => parse_global(ps, dot)
        DATA => parse_data(ps, dot)
        IC   => parse_ic(ps, dot)
        MEASURE => parse_measure(ps, dot)
        TRAN => parse_tran(ps, dot)
        PRINT => parse_print(ps, dot)
        TEMP => parse_temp(ps, dot)
        WIDTH => parse_width(ps, dot)
        HDL => parse_hdl(ps, dot)
        _ =>  error!(ps, UnexpectedToken)
    end
end

function parse_title(ps, dot)
    kw = take_kw(ps, TITLE)
    line = nothing
    if kind(nt(ps)) == TITLE_LINE
        line = take(ps, TITLE_LINE)
    end
    nl = accept_newline(ps)
    return EXPR(Title(dot, kw, line, nl))
end

function parse_title(ps)
    line = take(ps, TITLE_LINE)
    nl = accept_newline(ps)
    return EXPR(Title(nothing, nothing, line, nl))
end

function parse_tran(ps, dot)
    kw = take_kw(ps, TRAN)
    tstep_or_stop = EXPR(NumericValue(take_literal(ps), maybe_take_unit(ps)))
    tstop = if is_literal(kind(nt(ps)))
        tstep = tstep_or_stop
        EXPR(NumericValue(take_literal(ps), maybe_take_unit(ps)))
    else
        tstep = nothing
        tstep_or_stop
    end
    tstart = if is_literal(kind(nt(ps)))
        EXPR(NumericValue(take_literal(ps), maybe_take_unit(ps)))
    end
    tmax = if is_literal(kind(nt(ps)))
        EXPR(NumericValue(take_literal(ps), maybe_take_unit(ps)))
    end
    uic = if is_ident(kind(nt(ps)))
        take_identifier(ps)
    end
    nl = accept_newline(ps)
    if tstep === nothing && ps.lexer.strict && (ps.lexer.spice_dialect in (:ngspice, :hspice))
        error!(ps, StrictModeViolation)
    end
    return EXPR(Tran(dot, kw, tstep, tstop, tstart, tmax, uic, nl))
end

function parse_measure(ps, dot)
    kw = take_kw(ps, MEASURE)
    type = take_kw(ps, (AC, DC, OP, TRAN, TF, NOISE))
    name = if kind(nt(ps)) == STRING
        take_string(ps)
    else
        take_identifier(ps)
    end

    if kind(nt(ps)) in (FIND, DERIV, PARAMETERS, WHEN, AT)
        return parse_measure_point(ps, dot, kw, type, name)
    else
        return parse_measure_range(ps, dot, kw, type, name)
    end
end

function parse_trig_targ(ps)
    kw = take_kw(ps, (TRIG, TARG))
    lhs = parse_expression(ps)

    val = nothing
    if kind(nt(ps)) == VAL
        val_kw = take_kw(ps)
        eq = take(ps, EQ)
        rhs = parse_expression(ps)
        val = EXPR(Val_(val_kw, eq, rhs))
    end

    td = nothing
    if kind(nt(ps)) == TD
        td = parse_td(ps)
    end

    rfc = nothing
    if kind(nt(ps)) in (RISE, FALL, CROSS)
        rfc = parse_risefallcross(ps)
    end
    return EXPR(TrigTarg(kw, lhs, val, td, rfc))
end

function parse_measure_range(ps, dot, kw, type, name)
    avgmaxminpprmsinteg = nothing

    avgmaxminpprmsinteg = nothing
    if kind(nt(ps)) in (AVG, MAX, MIN, PP, RMS, INTEG)
        kw_op = take_kw(ps)
        expr = parse_expression(ps)
        avgmaxminpprmsinteg = EXPR(AvgMaxMinPPRmsInteg(kw_op, expr))
    end

    trig = nothing
    targ = nothing
    if kind(nt(ps)) == TRIG
        trig = parse_trig_targ(ps)
    end
    if kind(nt(ps)) == TARG
        targ = parse_trig_targ(ps)
    end

    nl = accept_newline(ps)
    return EXPR(MeasureRangeStatement(dot, kw, type, name, avgmaxminpprmsinteg, trig, targ, nl))
end

function parse_risefallcross(ps)
    rfc_kw = take_kw(ps, (RISE, FALL, CROSS))
    eq_rfc = take(ps, EQ)
    val = if kind(nt(ps)) == LAST
        take_kw(ps)
    else
        take_literal(ps)
    end
    return EXPR(RiseFallCross(rfc_kw, eq_rfc, val))
end

function parse_td(ps)
    td_kw = take_kw(ps, TD)
    eq_td = take(ps, EQ)
    expr = parse_expression(ps)
    return EXPR(TD_(td_kw, eq_td, expr))
end

function parse_measure_point(ps, dot, kw, type, name)
    fdp = nothing
    whenat = nothing
    td = nothing
    rfc = nothing
    if kind(nt(ps)) in (FIND, DERIV, PARAMETERS)
        fdp_kw = take_kw(ps, (FIND, DERIV, PARAMETERS))
        eq = kind(nt(ps)) == EQ ? take(ps, EQ) : nothing
        expr = parse_expression(ps)
        fdp = EXPR(FindDerivParam(fdp_kw, eq, expr))
    end
    if kind(nt(ps)) in (WHEN, AT)
        if kind(nt(ps)) == AT
            at_kw = take_kw(ps)
            eq = take(ps, EQ)
            at_expr = parse_expression(ps)
            whenat = EXPR(At(at_kw, eq, at_expr))
        else
            when_kw = take_kw(ps)
            when_expr = parse_expression(ps)
            whenat = EXPR(When(when_kw, when_expr))
        end
    end
    if kind(nt(ps)) in (RISE, FALL, CROSS)
        rfc = parse_risefallcross(ps)
    end
    if kind(nt(ps)) == TD
        td = parse_td(ps)
    end
    nl = accept_newline(ps)
    return EXPR(MeasurePointStatement(dot, kw, type, name, fdp, whenat, rfc, td, nl))
end


function parse_ic_statement(ps)
    if kind(nt(ps)) == STAR
        return EXPR(WildCard(nothing, take(ps, STAR)))
    elseif kind(nt(ps)) == INT_LIT
        int = take_literal(ps)
        if kind(nt(ps)) == STAR
            return EXPR(WildCard(int, take(ps, STAR)))
        end
        return int
    elseif is_ident(kind(nt(ps)))
        l = take_identifier(ps)
        if kind(nt(ps)) == COLON
            colon = take(ps, COLON)
            r = take_identifier(ps)
            return EXPR(Coloned(l, colon, r))
        end
        return l
    else
        error("unhandled ic statement")
    end
end

function parse_ic(ps, dot)
    kw = take_kw(ps, IC)
    entries = EXPRList{ICEntry}()
    while !eol(ps)
        name = take_identifier(ps)
        lparen = take(ps, LPAREN)
        arg = parse_ic_statement(ps)

        rparen = take(ps, RPAREN)
        eq = take(ps, EQ)
        val = parse_expression(ps)
        entry = EXPR(ICEntry(name, lparen, arg, rparen, eq, val))
        push!(entries, entry)
    end
    nl = accept_newline(ps)
    return EXPR(ICStatement(dot, kw, entries, nl))
end

function parse_print(ps, dot)
    kw = take_kw(ps, PRINT)
    entries = EXPRList{Any}()
    while !eol(ps)
        push!(entries, parse_expression(ps))
    end
    nl = accept_newline(ps)
    return EXPR(PrintStatement(dot, kw, entries, nl))
end

function parse_data(ps, dot)
    kw = take_kw(ps, DATA)
    blockname = take_identifier(ps)

    n_rows = 0
    row_names = EXPRList{Identifier}()
    while is_ident(kind(nt(ps)))
        push!(row_names, take_identifier(ps))
        n_rows += 1
    end

    values = EXPRList{NumericValue}()
    while !eol(ps)
        for i in 1:n_rows
            push!(values, EXPR(NumericValue(take_literal(ps), maybe_take_unit(ps))))
        end
    end

    nl = accept_newline(ps)
    dot2 = take(ps, DOT)
    endkw = take_kw(ps, ENDDATA)
    nl2 = accept_newline(ps)
    return EXPR(DataStatement(dot, kw, blockname, row_names, values, nl, dot2, endkw, nl2))
end

function parse_options(ps, dot)
    kw = take_kw(ps, OPTIONS)
    params = parse_parameter_list(ps)
    nl = accept_newline(ps)
    return EXPR(OptionStatement(dot, kw, params, nl))
end

function parse_width(ps, dot)
    kw = take_kw(ps, WIDTH)
    params = parse_parameter_list(ps)
    nl = accept_newline(ps)
    return EXPR(WidthStatement(dot, kw, params, nl))
end

function parse_node(ps)
    if kind(nt(ps)) == INT_LIT
        return EXPR(NodeName(take_literal(ps)))
    elseif is_ident(kind(nt(ps)))
        return EXPR(NodeName(take_identifier(ps)))
    else
        error!(ps, UnexpectedToken)
    end
end

function parse_node_list(ps)
    nodes = EXPRList{NodeName}()
    return parse_node_list!(nodes, ps)
end

function parse_node_list!(nodes, ps)
    while !eol(ps)
        p = parse_node(ps)
        push!(nodes, p)
    end
    return nodes
end


function parse_hierarchial_node(ps, name=parse_node(ps))
    subnodes = EXPRList{SubNode}()
    while kind(nt(ps)) == DOT
        dot = take(ps, DOT)
        subnode = parse_node(ps)
        push!(subnodes, EXPR(SubNode(dot, subnode)))
    end
    return EXPR(HierarchialNode(name, subnodes))
end

function parse_hierarchial_node_list(ps)
    nodes = EXPRList{HierarchialNode}()
    return parse_hierarchial_node_list!(nodes, ps)
end

function parse_hierarchial_node_list!(nodes, ps)
    while !eol(ps)
        p = parse_hierarchial_node(ps)
        push!(nodes, p)
    end
    return nodes
end

function parse_model(ps, dot)
    kw = take_kw(ps, MODEL)
    name = parse_hierarchial_node(ps)
    typ = take_identifier(ps)
    parameters = parse_parameter_list(ps)
    nl = accept_newline(ps)
    return EXPR(Model(dot, kw, name, typ, parameters, nl))
end

function parse_subckt(ps, dot)
    kw = take_kw(ps, SUBCKT)
    name = take_identifier(ps)
    nodes = EXPRList{NodeName}()
    parameters = EXPRList{Parameter}()
    while !eol(ps)
        if kind(nnt(ps)) == EQ
            parse_parameter_list!(parameters, ps)
        else
            push!(nodes, parse_node(ps))
        end
    end
    nl = accept_newline(ps)
    exprs = EXPRList{Any}()
    while true
        if kind(nt(ps)) == DOT
            dot2 = take(ps, DOT)
            if kind(nt(ps)) == ENDS
                ends = take_kw(ps, ENDS)
                name_end = eol(ps) ? nothing : take_identifier(ps)
                nl2 = accept_newline(ps)
                return EXPR(Subckt(dot, kw, name, nodes, parameters, nl, exprs, dot2, ends, name_end, nl2))
            else
                expr = parse_dot(ps, dot2)
            end
        else
            expr = parse_spice_toplevel(ps)
        end
        push!(exprs, expr)
    end
    error("unreachable")
end

function parse_dc(ps, dot)
    kw = take_kw(ps, DC)
    commands = EXPRList{DCCommand}()
    while !eol(ps)
        command = parse_dc_command(ps)
        push!(commands, command)
    end
    nl = accept_newline(ps)
    return EXPR(DCStatement(dot, kw, commands, nl))
end

function parse_dc_command(ps)
    name = take_identifier(ps)
    start = parse_expression(ps)
    stop = parse_expression(ps)
    step = parse_expression(ps)
    return EXPR(DCCommand(name, start, stop, step))
end

function parse_ac(ps, dot)
    kw = take_kw(ps, AC)
    command = parse_ac_command(ps)
    nl = accept_newline(ps)
    return EXPR(ACStatement(dot, kw, command, nl))
end

function parse_ac_command(ps)
    name = take_identifier(ps) # Should be keyword (lin/dec/oct)?
    n = parse_expression(ps)
    fstart = parse_expression(ps)
    fstop = parse_expression(ps)
    return EXPR(ACCommand(name, n, fstart, fstop))
end

function parse_endl(ps, dot)
    endd = take_kw(ps, ENDL)
    endl_id = nothing
    if is_ident(kind(nt(ps)))
        endl_id = take_identifier(ps)
    end
    nl = accept_newline(ps)
    return EXPR(EndlStatement(dot, endd, endl_id, nl))
end


function parse_lib(ps, dot)
    kw = take_kw(ps, LIB)
    # library section is of the form .lib identifier
    # library include is of the form .lib [string|identifier] identifier
    if !is_ident(kind(nnt(ps)))
        name = take_identifier(ps)
        nl = accept_newline(ps)
        stmt = nothing
        exprs = EXPRList{Any}()
        while true
            # TODO: What can be inside a lib statement?
            stmt = parse_spice_toplevel(ps)
            # Failed to parse toplevel, should have an endl here
            if stmt isa EXPR{EndlStatement}
                return EXPR(LibStatement(dot, kw, name, nl, exprs, stmt))
            end
            push!(exprs, stmt)
        end
    else
        path = take_path(ps)
        name = take_identifier(ps)
        nl = accept_newline(ps)
        return EXPR(LibInclude(dot, kw, path, name, nl))
    end
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

function parse_primary(ps)
    if is_number(kind(nt(ps)))
        lit = take_literal(ps)
        # TODO: Units?
        return EXPR(NumericValue(lit, maybe_take_unit(ps)))
    elseif is_literal(kind(nt(ps)))
        return take_literal(ps)
    elseif is_ident(kind(nt(ps)))
        id = take_identifier(ps)
        if kind(nt(ps)) == LPAREN
            return parse_function_call(ps, id)
        elseif kind(nt(ps)) == DOT
            return parse_hierarchial_node(ps, EXPR(NodeName(id)))
        else
            return id
        end
    elseif kind(nt(ps)) == STRING
        return take_string(ps)
    elseif kind(nt(ps)) == LSQUARE
        return parse_array(ps)
    elseif is_kw(kind(nt(ps)))
        return take_kw(ps)
    elseif kind(nt(ps)) == LBRACE
        lparen = take(ps, LBRACE)
        e = parse_expression(ps)
        rparen = accept(ps, RBRACE)
        return EXPR(Brace(lparen, e, rparen))
    elseif kind(nt(ps)) == LPAREN
        lparen = take(ps, LPAREN)
        e = parse_expression(ps)
        rparen = accept(ps, RPAREN)
        return EXPR(Parens(lparen, e, rparen))
    elseif kind(nt(ps)) == PRIME
        lparen = take(ps, PRIME)
        e = parse_expression(ps)
        rparen = accept(ps, PRIME)
        return EXPR(Prime(lparen, e, rparen))
    elseif kind(nt(ps)) == JULIA_ESCAPE_BEGIN
        # Switch to julia parser
        contents = ps.srcfile.contents
        # TODO: Could we have a base interface that doesn't require a copy?
        thispos = ps.nnt.startbyte+1
        (je, newpos) = Base.Meta.parse(String(isa(contents, IOBuffer) ? copy(contents.data) : contents), thispos-1; raise=false, greedy=false)
        Base.seek(ps.lexer, newpos-2)
        ps.nnt = Token(JULIA_ESCAPE, Int64(ps.nnt.startbyte), newpos-3)
        ps.nnpos += newpos - thispos - 1
        ps.tok_storage = next_token(ps.lexer)
        open = take(ps, JULIA_ESCAPE_BEGIN)
        body = take_julia_escape_body(ps)
        close = accept(ps, RPAREN)
        return EXPR(JuliaEscape(open, body, close))
    end
    return error!(ps, UnexpectedToken)
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

function parse_array(ps)
    lsquare = accept(ps, LSQUARE)
    args = EXPRList{Any}()
    while kind(nt(ps)) != RSQUARE
        push!(args, parse_expression(ps))
    end
    rsquare = accept(ps, RSQUARE)
    return EXPR(Square(lsquare, args, rsquare))
end

function parse_expression(ps)
    if kind(nt(ps)) == PRIME
        lprime = take(ps, PRIME)
        expr = parse_expression(ps)
        rprime = take(ps, PRIME)
        return EXPR(Prime(lprime, expr, rprime))
    end
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

function parse_simulator(ps)
    kw = take_kw(ps, SIMULATOR)
    langkw = take_kw(ps, LANG)
    eq = take(ps, EQ)
    if kind(nt(ps)) == SPECTRE
        ps.lang_swapped=true
    end
    lang = take_kw(ps, (SPECTRE, SPICE))
    params = parse_parameter_list(ps)
    nl = accept_newline(ps)
    return EXPR(Simulator(kw, langkw, eq, lang, params, nl))
end


function parse_parameter_list(ps)
    parameters = EXPRList{Parameter}()
    return parse_parameter_list!(parameters, ps)
end

function parse_parameter_list!(parameters, ps)
    while !eol(ps) && kind(nt(ps)) != RPAREN
        p = parse_parameter(ps)
        push!(parameters, p)
    end
    return parameters
end


function parse_parameter_mod(ps)
    # TODO: Lot
    kw = take_kw(ps, DEV)
    slash, distr = nothing, nothing
    if kind(nt(ps)) == SLASH
        slash = take(ps, SLASH)
        distr = take_identifier(ps)
    end
    eq = take(ps, EQ)
    val = parse_expression(ps)
    return EXPR(DevMod(kw, slash, distr, eq, val))
end

function parse_parameter(ps, name = take_identifier(ps))
    eq = nothing
    val = nothing
    mod = nothing
    if kind(nt(ps)) == EQ # flags like savecurrents
        eq = accept(ps, EQ)
        val = parse_expression(ps)
    end
    if kind(nt(ps)) == DEV
        mod = parse_parameter_mod(ps)
    end
    return EXPR(Parameter(name, eq, val, mod))
end


function parse_param(ps, dot)
    kw = take_kw(ps)
    params = parse_parameter_list(ps)
    nl = accept_newline(ps)
    return EXPR(ParamStatement(dot, kw, params, nl))
end

function parse_temp(ps, dot)
    kw = take_kw(ps)
    val = parse_expression(ps)
    nl = accept_newline(ps)
    return EXPR(TempStatement(dot, kw, val, nl))
end

function parse_global(ps, dot)
    kw = take_kw(ps, GLOBAL)
    nodes = parse_node_list(ps)
    nl = accept_newline(ps)
    return EXPR(GlobalStatement(dot, kw, nodes, nl))
end

function parse_csparam(ps, dot)
    kw = take_kw(ps)
    params = parse_parameter_list(ps)
    nl = accept_newline(ps)
    return EXPR(ParamStatement(dot, kw, params, nl))
end


function parse_include(ps, dot)
    kw = take_kw(ps, INCLUDE)
    path = take_path(ps)
    nl = accept_newline(ps)
    return EXPR(IncludeStatement(dot, kw, path, nl))
end

function parse_hdl(ps, dot)
    kw = take_kw(ps, HDL)
    path = take_path(ps)
    nl = accept_newline(ps)
    return EXPR(HDLStatement(dot, kw, path, nl))
end


function parse_end(ps, dot)
    endd = take_kw(ps, END)
    nl = accept_newline(ps)
    return EXPR(EndStatement(dot, endd, nl))
end

function parse_instance(ps)
    @case kind(nt(ps)) begin
        IDENTIFIER_RESISTOR => parse_resistor(ps)
        IDENTIFIER_VOLTAGE => parse_voltage(ps)
        IDENTIFIER_CURRENT => parse_current(ps)
        IDENTIFIER_VOLTAGE_CONTROLLED_CURRENT => parse_voltage_controlled(VCCS, ps)
        IDENTIFIER_VOLTAGE_CONTROLLED_VOLTAGE => parse_voltage_controlled(VCVS, ps)
        IDENTIFIER_CURRENT_CONTROLLED_CURRENT => parse_current_controlled(CCCS, ps)
        IDENTIFIER_CURRENT_CONTROLLED_VOLTAGE => parse_current_controlled(CCVS, ps)
        IDENTIFIER_BEHAVIORAL => parse_behavioral(ps)
        IDENTIFIER_MOSFET => parse_mosfet(ps)
        IDENTIFIER_S_PARAMETER_ELEMENT => parse_s_parameter_element(ps)
        IDENTIFIER_SWITCH => parse_switch(ps)
        IDENTIFIER_DIODE => parse_diode(ps)
        IDENTIFIER_CAPACITOR => parse_capacitor(ps)
        IDENTIFIER_LINEAR_INDUCTOR => parse_inductor(ps)
        IDENTIFIER_SUBCIRCUIT_CALL => parse_subckt_call(ps)
        IDENTIFIER_BIPOLAR_TRANSISTOR => parse_bipolar_transistor(ps)
        _ => error!(ps, UnexpectedToken)
    end
end

function parse_julia_device(ps, name, nodes...)
    ns = EXPRList{NodeName}()
    for n in nodes
        push!(ns, n)
    end
    dev = parse_primary(ps)
    nl = accept_newline(ps)
    return EXPR(JuliaDevice(name, ns, dev, nl))
end

function parse_inductor(ps)
    name = parse_node(ps)
    pos = parse_node(ps)
    neg = parse_node(ps)
    kind(nt(ps)) == JULIA_ESCAPE_BEGIN && return parse_julia_device(ps, name, pos, neg)
    val = kind(nnt(ps)) == EQ ? nothing : parse_expression(ps)
    params = parse_parameter_list(ps)
    nl = accept_newline(ps)
    return EXPR(Inductor(name, pos, neg, val, params, nl))
end

function parse_voltage_controlled(cs, ps)
    name = parse_node(ps)
    pos = parse_node(ps)
    neg = parse_node(ps)
    kind(nt(ps)) == JULIA_ESCAPE_BEGIN && return parse_julia_device(ps, name, pos, neg)
    cpos = kind(nnt(ps)) == EQ ? nothing : parse_node(ps)
    cneg = kind(nnt(ps)) == EQ ? nothing : parse_node(ps)
    val = kind(nnt(ps)) == EQ ? nothing : parse_expression(ps)
    params = parse_parameter_list(ps)
    nl = accept_newline(ps)
    return EXPR(cs(name, pos, neg, cpos, cneg, val, params, nl))
end

function parse_current_controlled(cs, ps)
    name = parse_node(ps)
    pos = parse_node(ps)
    neg = parse_node(ps)
    kind(nt(ps)) == JULIA_ESCAPE_BEGIN && return parse_julia_device(ps, name, pos, neg)
    vnam = kind(nnt(ps)) == EQ ? nothing : parse_node(ps)
    val = kind(nnt(ps)) == EQ ? nothing : parse_expression(ps)
    params = parse_parameter_list(ps)
    nl = accept_newline(ps)
    return EXPR(cs(name, pos, neg, vname, val, params, nl))
end

function parse_behavioral(ps)
    name = parse_node(ps)
    pos = parse_node(ps)
    neg = parse_node(ps)
    kind(nt(ps)) == JULIA_ESCAPE_BEGIN && return parse_julia_device(ps, name, pos, neg)
    params = parse_parameter_list(ps)
    nl = accept_newline(ps)
    return EXPR(Behavioral(name, pos, neg, params, nl))
end

parse_voltage(ps) = parse_voltage_or_current(ps, true)
parse_current(ps) = parse_voltage_or_current(ps, false)

function parse_tran_fn(ps)
    kw = take_kw(ps)
    # TODO: Check divisible by two?
    vals = EXPRList{Any}()
    while !eol(ps) && !is_kw(kind(nt(ps)))
        ref = parse_expression(ps)
        push!(vals, ref)
    end
    return EXPR(TranSource(kw, vals))
end

function parse_voltage_or_current(ps, isvoltage)
    name = parse_node(ps)
    pos = parse_node(ps)
    neg = parse_node(ps)
    kind(nt(ps)) == JULIA_ESCAPE_BEGIN && return parse_julia_device(ps, name, pos, neg)
    vals = EXPRList{Union{ACSource, DCSource, TranSource}}()
    while !eol(ps)
        if kind(nt(ps)) == DC
            dc = take_kw(ps, DC)
            eq = if kind(nt(ps)) == EQ
                take(ps, EQ)
            end
            expr = parse_expression(ps)
            push!(vals, EXPR(DCSource(dc, eq, expr)))
        elseif kind(nt(ps)) == AC
            ac = take_kw(ps, AC)
            eq = if kind(nt(ps)) == EQ
                take(ps, EQ)
            end
            expr = parse_expression(ps)
            # TODO Phase
            push!(vals, EXPR(ACSource(ac, eq, expr)))
        elseif is_source_type(kind(nt(ps)))
            push!(vals, parse_tran_fn(ps))
        else
            expr = parse_expression(ps)
            push!(vals, EXPR(DCSource(nothing, nothing, expr)))
        end
    end
    nl = accept_newline(ps)
    T = isvoltage ? Voltage : Current
    return EXPR(T(name, pos, neg, vals, nl))
end


function convert_node_expr_to_identifier(node::EXPR{NodeName})
    return EXPR{Identifier}(node.fullwidth, node.off, node.width, node.name.form)
end

function parse_bipolar_transistor(ps)
    name = parse_node(ps)
    c = parse_node(ps)
    b = parse_node(ps)
    e = parse_node(ps)
    kind(nt(ps)) == JULIA_ESCAPE_BEGIN && return parse_julia_device(ps, name, c, b, e)
    s = parse_node(ps) # or model
    kind(nt(ps)) == JULIA_ESCAPE_BEGIN && return parse_julia_device(ps, name, c, b, e, s)
    if is_ident(kind(nt(ps)))
        model = take_identifier(ps)
        if kind(nt(ps)) == EQ
            param_name = model
            model = convert_node_expr_to_identifier(s)
            s = nothing
            params = EXPRList{Parameter}()
            push!(params, parse_parameter(ps, param_name))
            parse_parameter_list!(params, ps)
        else
            params = parse_parameter_list(ps)
        end
    else
        model = convert_node_expr_to_identifier(s)
        s = nothing
        params = EXPRList{Parameter}()
    end
    nl = accept_newline(ps)
    return EXPR(BipolarTransistor(name, c, b, e, s, model, params, nl))
end

function parse_capacitor(ps)
    name = parse_node(ps)
    pos = parse_node(ps)
    neg = parse_node(ps)
    kind(nt(ps)) == JULIA_ESCAPE_BEGIN && return parse_julia_device(ps, name, pos, neg)
    val = kind(nnt(ps)) == EQ ? nothing : parse_expression(ps)
    params = parse_parameter_list(ps)
    nl = accept_newline(ps)
    return EXPR(Capacitor(name, pos, neg, val, params, nl))
end


function parse_diode(ps)
    name = parse_node(ps)
    pos = parse_node(ps)
    neg = parse_node(ps)
    kind(nt(ps)) == JULIA_ESCAPE_BEGIN && return parse_julia_device(ps, name, pos, neg)

    model = take_identifier(ps)
    params = parse_parameter_list(ps)
    nl = accept_newline(ps)
    return EXPR(Diode(name, pos, neg, model, params, nl))
end

function parse_resistor(ps)
    name = parse_node(ps)
    pos = parse_node(ps)
    neg = parse_node(ps)
    kind(nt(ps)) == JULIA_ESCAPE_BEGIN && return parse_julia_device(ps, name, pos, neg)
    val = kind(nnt(ps)) == EQ ? nothing : parse_expression(ps)
    params = parse_parameter_list(ps)
    nl = accept_newline(ps)
    return EXPR(Resistor(name, pos, neg, val, params, nl))
end


function parse_mosfet(ps)
    name = parse_node(ps)
    d = parse_node(ps)
    g = parse_node(ps)
    s = parse_node(ps)
    b = parse_node(ps)
    kind(nt(ps)) == JULIA_ESCAPE_BEGIN && return parse_julia_device(ps, name, d, g, s, b)
    model = parse_node(ps)
    parameters = parse_parameter_list(ps)
    nl = accept_newline(ps)
    return EXPR(MOSFET(name, d, g, s, b, model, parameters, nl))
end

function parse_subckt_call(ps)
    name = parse_node(ps)
    nodes = EXPRList{NodeName}()
    while kind(nnt(ps)) !== EQ && kind(nt(ps)) !== NEWLINE && kind(nt(ps)) !== JULIA_ESCAPE_BEGIN
        push!(nodes, parse_node(ps))
    end
    kind(nt(ps)) == JULIA_ESCAPE_BEGIN && return parse_julia_device(ps, name, nodes...)
    model = pop!(nodes)
    parameters = parse_parameter_list(ps)
    nl = accept_newline(ps)
    return EXPR(SubcktCall(name, nodes, model, parameters, nl))
end

function parse_s_parameter_element(ps)
    name = parse_node(ps)
    nd1 = parse_node(ps)
    nd2 = parse_node(ps)
    model = parse_node(ps)
    parameters = parse_parameter_list(ps)
    nl = accept_newline(ps)
    return EXPR(SParameterElement(name, nd1, nd2, model, parameters, nl))
end

function parse_switch(ps)
    name = parse_node(ps)
    nd1 = parse_node(ps)
    nd2 = parse_node(ps)
    cnd1 = parse_node(ps)
    cnd2 = parse_node(ps)
    kind(nt(ps)) == JULIA_ESCAPE_BEGIN && return parse_julia_device(ps, name, nd1, nd2, cnd1, cnd2)
    model = parse_node(ps)
    onoff = take_kw(ps)
    nl = accept_newline(ps)
    return EXPR(Switch(name, nd1, nd2, cnd1, cnd2, model, onoff, nl))
end

eol(ps) = (t = nt(ps); kind(t) == NEWLINE || kind(t) == ENDMARKER)


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

function maybe_take_unit(ps)
    if kind(nt(ps)) == UNIT
        EXPR!(UnitLiteral(), ps)
    end
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
    @assert kind(nt(ps)) == STRING "Expected STRING but found $(kind(nt(ps)))"
    return EXPR!(StringLiteral(), ps)
end

function take_path(ps)
    # an unquoted path might be lexed as an identifier
    @assert is_ident(kind(nt(ps))) || kind(nt(ps)) == STRING "Expected STRING or IDENTIFIER but found $(kind(nt(ps)))"
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

function take_julia_escape_body(ps)
    @assert kind(nt(ps)) == JULIA_ESCAPE
    return EXPR!(JuliaEscapeBody(), ps)
end

function error!(ps, kind, expected=nothing)
    # TODO Make the errors emit error tokens and show all of the errors as in VerilogParser
    ps.errored = true
    # debug && error("error token with kind $(nt(ps))")
    throw(SPICEParserError(ps, kind, expected))
end

struct SPICEParserError <: Exception
    ps
    kind
    expected
end

function Base.showerror(io::IO, s::SPICEParserError)
    ps = s.ps
    nt = ps.nt
    lb = LineBreaking(UInt64(0), ps.srcfile.lineinfo, nothing)
    line, col1 = LineNumbers.indtransform(lb, nt.startbyte)
    _, col2 = LineNumbers.indtransform(lb, nt.endbyte)
    line_str = String(ps.srcfile.lineinfo[line])
    print(io, "SPICEParser error at line $(line):")
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
    elseif opkind in (EQ, EQEQ, NOT_EQ, EQEQEQ, NOT_EQEQ)
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
