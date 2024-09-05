function parse_attr_spec(ps)
    if iskeyword(kind(nt(ps)))
        id = take_kw(ps)
    else
        id = accept_identifier(ps)
    end
    eq = accept(ps, EQ)
    val = parse_constant_expression(ps)
    EXPR(AttrSpec(id, eq, val))
end

function maybe_parse_attributes(ps)
    if kind(nt(ps)) != LATTR
        return nothing
    end
    lattr = take(ps, LATTR)
    items = EXPRList{ListItem{EXPR{AttrSpec}}}()
    first = EXPR(ListItem{EXPR{AttrSpec}}(nothing, parse_attr_spec(ps)))
    push!(items, first)
    while kind(nt(ps)) == COMMA
        comma = take(ps, COMMA)
        push!(items, EXPR(ListItem{EXPR{AttrSpec}}(comma, parse_attr_spec(ps))))
    end
    rattr = accept(ps, RATTR)
    EXPR(Attributes(lattr, items, rattr))
end

function parse_verilog_source(ps)
    attrs = maybe_parse_attributes(ps)
    return @case kind(nt(ps)) begin
        MODULE | MACROMODULE | CONNECTMODULE => parse_module(ps)
        PRIMITIVE => parse_primitive(ps)
        CONFIG => parse_config(ps)
        PARAMSET => parse_paramset(ps)
        DISCIPLINE => parse_discipline(ps)
        NATURE => parse_nature(ps)
        CONNECTRULES => parse_connectrules(ps)
        _ => error!(ps, UnexpectedToken)
    end
end

function take_kw(ps)
    kwkind = kind(nt(ps))
    @assert iskeyword(kwkind)
    EXPR!(Keyword(kwkind), ps)
end

function take_kw(ps, tkind)
    !isa(tkind, Tuple) && (tkind = (tkind,))
    kwkind = kind(nt(ps))
    @assert iskeyword(kwkind) && kwkind in tkind
    EXPR!(Keyword(kwkind), ps)
end

function accept_identifier_part(ps)
    if !(kind(nt(ps)) in (IDENTIFIER, ESCD_IDENTIFIER))
        consume = kind(nt(ps)) in (SEMICOLON,) # TODO: What's a good list here?
        return handle_unexpected_token!(ps, IDENTIFIER, consume)
    end
    if kind(nt(ps)) == IDENTIFIER
        return EXPR!(IdentifierPart(false), ps)
    else
        return EXPR!(IdentifierPart(true), ps)
    end
end

function accept_identifier(ps)
    id = accept_identifier_part(ps)
    if kind(nt(ps)) != BACKTICK_BACKTICK
        return EXPR(Identifier(id, nothing))
    end
    ## This is a SystemVerilog Extension
    list = EXPRList{IdentifierConcatItem}()
    while kind(nt(ps)) == BACKTICK_BACKTICK
        bb = take(ps, BACKTICK_BACKTICK)
        id′ = accept_identifier_part(ps)
        push!(list, EXPR(IdentifierConcatItem(bb, id′)))
    end
    return EXPR(Identifier(id, list))
end

function take_system_identifier(ps)
    @assert kind(nt(ps)) == SYSTEM_IDENTIFIER
    return EXPR!(SystemIdentifier(), ps)
end

function take_literal(ps)
    ntkind = kind(nt(ps))
    @assert isliteral(ntkind)
    EXPR!(ntkind == FLOAT ? FloatLiteral() : Literal(), ps)
end

function accept(ps, tkind)
    !isa(tkind, Tuple) && (tkind = (tkind,))
    if kind(nt(ps)) in tkind
        return EXPR!(Notation(), ps)
    else
        return error!(ps, UnexpectedToken, tkind, kind(nt(ps)))
    end
end

function accept_noconsume(ps, tkind)
    !isa(tkind, Tuple) && (tkind = (tkind,))
    if kind(nt(ps)) in tkind
        return EXPR!(Notation(), ps)
    else
        return error!(ps, UnexpectedToken, tkind, kind(nt(ps)), false)
    end
end

function accept_kw(ps, tkind)
    !isa(tkind, Tuple) && (tkind = (tkind,))
    @assert all(iskeyword, tkind)
    kwkind = kind(nt(ps))
    if kwkind in tkind
        return EXPR!(Keyword(kwkind), ps)
    else
        return error!(ps, UnexpectedToken, tkind, kind(nt(ps)), false)
    end
end

function accept_end(ps, tkind)
    !isa(tkind, Tuple) && (tkind = (tkind,))
    @assert all(iskeyword, tkind)
    kwkind = kind(nt(ps))
    if kwkind in tkind
        return EXPR!(Keyword(kwkind), ps)
    else
        return error!(ps, UnexpectedToken, tkind, kind(nt(ps)), kind(nt(ps)) in (END, SEMICOLON))
    end
end

function accept_kw(ps)
    kwkind = kind(nt(ps))
    if iskeyword(kwkind)
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
    @assert isoperator(ntkind)
    return EXPR!(Operator(ntkind), ps)
end

function take_assignment_op(ps)
    ntkind = kind(nt(ps))
    @assert is_sva_eq(ntkind) || ntkind == EQ
    return EXPR!(Operator(ntkind), ps)
end

function take_builtin_func(ps)
    @assert is_filter_func(kind(nt(ps))) || is_small_sig_func(kind(nt(ps)))
    return EXPR!(BuiltinFunc(), ps)
end

function error!(ps, kind, context = nothing, got = nothing, consume = true)
    ps.errored = true
    if !consume
        return EXPR(UInt32(0), UInt32(0), UInt32(0), Error(kind, context, got))
    else
        return EXPR!(Error(kind, context, got), ps)
    end
end

function parse_identifier_list(ps, first_ident)
    ports = EXPRList{IdentifierListItem}()
    push!(ports, EXPR(IdentifierListItem(nothing, first_ident)))
    while kind(nt(ps)) == COMMA && !(kind(nt(ps, 2)) in (INPUT, OUTPUT, INOUT))
        push!(ports, EXPR(IdentifierListItem(take(ps, COMMA), accept_identifier(ps))))
    end
    return ports
end


function parse_inout_declaration(ps)
    kw = take_kw(ps, (INPUT, OUTPUT, INOUT))
    disc_or_port = discipline = net_type = signed = range = nothing

@label discipline
    kind(nt(ps)) == LSQUARE && @goto range
    iskeyword(kind(nt(ps))) && @goto net_type
    disc_or_port = accept_identifier(ps)

@label net_type
    kind(nt(ps)) == LSQUARE && @goto range
    (disc_or_port !== nothing && kind(nt(ps)) == COMMA) && @goto port
    if iskeyword(kind(nt(ps)))
        (is_net_type(kind(nt(ps))) || kind(nt(ps)) == WREAL) || @goto signed
        net_type = take_kw(ps)
    end

@label signed
    kind(nt(ps)) == IDENTIFIER && @goto port
    (disc_or_port !== nothing && kind(nt(ps)) == COMMA) && @goto port
    if kind(nt(ps)) == SIGNED
        signed = take_kw(ps, SIGNED)
    end

@label range
    kind(nt(ps)) == LSQUARE || @goto port
    range = parse_range(ps)

@label port
    if !isident(kind(nt(ps)))
        net_type !== nothing || signed !== nothing && error!(ps, UnexpectedToken)
        port = disc_or_port
    else
        discipline = disc_or_port
        port = accept_identifier(ps)
    end

    ports = parse_identifier_list(ps, port)

    return EXPR(InOutDeclaration(kw, discipline, net_type, signed, range, ports))
end

function parse_intreal_declaration(ps)
    kw = take_kw(ps, (INTEGER, REAL))
    return EXPR(IntRealDeclaration(kw, parse_identifier_list(ps, accept_identifier(ps))))
end

function parse_net_declaration(ps)
    discipline = accept_identifier(ps)
    if kind(nt(ps)) == LSQUARE
        range = parse_range(ps)
    end
    is_assignemnts = kind(nt(ps)) == IDENTIFIER && kind(nt(ps, 2)) == EQ
    net_names = is_assignemnts ?
        EXPRList{NetAssignmentListItem}() :
        EXPRList{NetNameListItem}()
    while true
        comma = kind(nt(ps)) == COMMA ? take(ps, COMMA) : nothing
        id = accept_identifier(ps)
        if is_assignemnts
            accept(ps, EQ)
            # TODO: This is wrong - not technically an analog expression
            expr = parse_analog_expression(ps)
            push!(net_names, EXPR(NetAssignmentListItem(comma, expr)))
        else
            push!(net_names, EXPR(NetNameListItem(comma, id)))
        end
        first = false
        kind(nt(ps)) !== COMMA && break
    end
    return EXPR(NetDeclaration(nothing, discipline, net_names, accept(ps, SEMICOLON)))
end

function parse_primary(ps, isconst::Bool)
    if isliteral(kind(nt(ps)))
        lit = take_literal(ps)
        kind(nt(ps)) in (BASE_SPEC, HEX_BASE_SPEC) && @goto base_spec
        return lit
    end
    ntkind = kind(nt(ps))
    if isident(ntkind) || is_filter_func(ntkind) || is_small_sig_func(ntkind) || ntkind == SYSTEM_IDENTIFIER
        if isident(ntkind)
            id = accept_identifier(ps)
        elseif is_filter_func(ntkind) || is_small_sig_func(ntkind)
            id = take_builtin_func(ps)
        else
            id = take_system_identifier(ps)
        end
        attr = maybe_parse_attributes(ps)
        if kind(nt(ps)) == LPAREN
            attr === nothing || error(ps, "TODO")
            return parse_function_call(isconst ? parse_constant_mintypmax_expression : parse_analog_expression, ps, id)
        elseif attr !== nothing
            error(ps)
        end

        if ntkind == SYSTEM_IDENTIFIER
            return id
        end

        if kind(nt(ps)) == DOT
            # nature_attribute_reference
            error("TODO: nature_attribute_reference not supported.")
        end
        range = nothing
        if kind(nt(ps)) == LSQUARE
            range = parse_range(ps)
        end
        return EXPR(IdentifierPrimary(id, range))
    end
    if kind(nt(ps)) == LPAREN
        lparen = take(ps, LPAREN)
        e = (isconst ? parse_constant_mintypmax_expression : parse_analog_expression)(ps)
        rparen = accept(ps, RPAREN)
        return EXPR(Parens(lparen, e, rparen))
    end
    if kind(nt(ps)) == LBRACE
        # constant_concatenation, constant_multiple_concatenation
        error("TODO: constant_concatenation, constant_multiple_concatenation not supported.")
    end
    if kind(nt(ps)) in (BASE_SPEC, HEX_BASE_SPEC)
        lit = nothing
@label base_spec
        # XXX: Validate these
        bp = accept(ps, (BASE_SPEC, HEX_BASE_SPEC))
        if kind(nt(ps)) == IDENTIFIER
            # N.B. There is a lexing ambiguity here, since any of these tokens
            # could have been macro substituted.
            digs = EXPR!(Literal(), ps)
        elseif kind(nt(ps)) in (INT, HEX_INT)
            digs = take_literal(ps)
        else
            error(ps, UnexpectedToken)
        end
        return EXPR(BasedInteger(lit, bp, digs))
    end
    if kind(nt(ps)) == STRING
        return take_string(ps)
    end
    if kind(nt(ps)) in (RPAREN, RBRACE, RSQUARE, COMMA, SEMICOLON, BACKTICK, EQ, LATTR, RATTR, ERROR, COLON, CASSIGN, ENDMARKER, RESERVED) || iskeyword(kind(nt(ps))) || isoperator(kind(nt(ps))) || is_sva_eq(kind(nt(ps)))
        return error!(ps, UnexpectedToken, nothing, kind(nt(ps)))
    end
    if kind(nt(ps)) in (PREPROC_ERR_UNDEF_MACRO,)
        return error!(ps, UndefinedMacro)
    elseif kind(nt(ps)) in (PREPROC_ERR_RECURSIVE_MACRO,)
        return error!(ps, RecursiveMacro)
    elseif kind(nt(ps)) in (PREPROC_ERR_MISSING_MACRO,)
        return error!(ps, MissingMacro, nothing, kind(nt(ps, 2)))
    end
    error(ps, "TODO")
end

function parse_unary_op(ps, isconst::Bool)
    if !is_unary_operator(kind(nt(ps)))
        return error!(ps, UnexpectedToken, nothing, kind(nt(ps)))
    end
    op = take_operator(ps)
    attrs = maybe_parse_attributes(ps)
    primary = parse_primary(ps, false)
    return EXPR(UnaryOp(op, attrs, primary))
end

function parse_primary_or_unary(ps, isconst)
    if isoperator(kind(nt(ps)))
        parse_unary_op(ps, isconst)
    else
        parse_primary(ps, isconst)
    end
end

function parse_binop(ps, ex, op, opterm = nothing, isconst=false)
    local attrs, rhs
    while true
        attrs = maybe_parse_attributes(ps)
        rhs = parse_primary_or_unary(ps, isconst)
        is_binary_operator(kind(nt(ps))) || break
        ntprec = prec(kind(nt(ps)))
        if prec(op) >= ntprec
            ex = EXPR(BinaryExpression(ex, op, attrs, rhs))
            (opterm !== nothing && prec(opterm) >= ntprec) && return ex
            op = take_operator(ps)
            continue
        else
            rhs = parse_binop(ps, rhs, take_operator(ps), op, isconst)
            ex = EXPR(BinaryExpression(ex, op, attrs, rhs))
            is_binary_operator(kind(nt(ps))) || return ex
            op = take_operator(ps)
            continue
        end
    end
    ret = EXPR(BinaryExpression(ex, op, attrs, rhs))
    return ret
end

function parse_constant_expression(ps)
    ex = parse_primary_or_unary(ps, true)
    if isoperator(kind(nt(ps)))
        op = take_operator(ps)
        ex = parse_binop(ps, ex, op, nothing, true)
    end
    if kind(nt(ps)) == CONDITIONAL
        not = take(ps, CONDITIONAL)
        ifcase = parse_constant_expression(ps)
        colon = accept(ps, COLON)
        elsecase = parse_constant_expression(ps)
        ex = EXPR(TernaryExpr(ex, not, ifcase, colon, elsecase))
    end
    return ex
end

function parse_analog_expression(ps, ex=parse_primary_or_unary(ps, false))
    if isoperator(kind(nt(ps)))
        op = take_operator(ps)
        ex = parse_binop(ps, ex, op, nothing, false)
    end
    if kind(nt(ps)) == CONDITIONAL
        not = take(ps, CONDITIONAL)
        ifcase = parse_analog_expression(ps)
        colon = accept(ps, COLON)
        elsecase = parse_analog_expression(ps)
        ex = EXPR(TernaryExpr(ex, not, ifcase, colon, elsecase))
    end
    return ex
end

function parse_constant_mintypmax_expression(ps)
    a = parse_constant_expression(ps)
    kind(nt(ps)) == COLON || return a
    cc1 = take(nt(ps), COLON)
    b = parse_constant_expression(ps)
    cc2 = accept(nt(ps), COLON)
    b = parse_constant_expression(ps)
    return EXPR(CMinTypMaxExpr(a, cc1, b, cc2, c))
end

function parse_range_bound(ps, is_value_range::Bool)
    # Value range also allows inf or -inf
    if is_value_range
        if kind(nt(ps)) == INF
            return take_kw(ps, INF)
        elseif kind(nt(ps)) == MINUS && kind(nt(ps, 2)) == INF
            min = take_operator(ps)
            kw = take_kw(ps, INF)
            # Not how the standard represents this, but shouldn't cause trouble
            return EXPR(UnaryOp(min, nothing, kw))
        end
    end
    return parse_constant_expression(ps)
end

function parse_range(ps, is_value_range::Bool)
    lsquare = take(ps, is_value_range ? (LSQUARE, LPAREN) : LSQUARE)
    min = parse_range_bound(ps, is_value_range)
    colon = accept(ps, COLON)
    max = parse_range_bound(ps, is_value_range)
    rsquare = accept(ps, is_value_range ? (RSQUARE, RPAREN) : RSQUARE)
    return EXPR(RangeSpec(lsquare, min, colon, max, rsquare))
end

function parse_aliasparameter_declaration(ps)
    kw = take_kw(ps, ALIASPARAM)
    id = accept_identifier(ps)
    eq = accept(ps, EQ)
    val = accept_identifier(ps)
    return EXPR(AliasParameterDeclaration(kw, id, eq, val))
end

function parse_parameter_declaration(ps)
    kw = take_kw(ps, PARAMETER)
    parameter_type = range = nothing
    if iskeyword(kind(nt(ps)))
        issigned = kind(nt(ps)) == SIGNED
        if !is_parameter_type(kind(nt(ps))) && !issigned
            error(ps, UnexpectedToken)
        end
        parameter_type = accept_kw(ps)
        issigned || @goto norange
    end

    if kind(nt(ps)) == LSQUARE
        range = parse_range(ps)
    end

@label norange

    params = EXPRList{ParamAssignmentListItem}()
    comma = nothing
    while true
        id = accept_identifier(ps)
        prange = nothing
        if kind(nt(ps)) == LSQUARE
            prange = parse_range(ps)
        end
        eq = accept(ps, EQ)
        @assert prange === nothing # TODO
        expr = parse_constant_mintypmax_expression(ps)
        vrss = EXPRList{ValueRange}()
        while kind(nt(ps)) in (FROM, EXCLUDE)
            vtype = take_kw(ps)
            if kind(nt(ps)) in (LSQUARE, LPAREN)
                vrs = parse_range(ps, true)
            elseif kind(nt(ps)) == AP_LBRACE
                error("TODO")
            else
                vrs = parse_constant_expression(ps)
            end
            push!(vrss, EXPR(ValueRange(vtype, vrs)))
        end
        push!(params, EXPR(ParamAssignmentListItem(comma,
            EXPR(ParamAssignment(id, prange, eq, expr, vrss)))))
        kind(nt(ps)) == COMMA || break
        comma = take(ps, COMMA)
    end

    return EXPR(ParameterDeclaration(kw, parameter_type, range, params))
end

function parse_if(parse_statement, ps)
    kw = take_kw(ps, IF)
    lparen = accept_noconsume(ps, LPAREN)
    expr = parse_analog_expression(ps)
    rparen = accept_noconsume(ps, RPAREN)
    if kind(nt(ps)) == SEMICOLON
        stmt = take(ps, SEMICOLON)
    else
        stmt = parse_statement(ps)
    end
    return EXPR(AnalogIf(kw, lparen, expr, rparen, stmt))
end

function parse_analog_conditional_block(parse_statement, ps)
    aif = parse_if(parse_statement, ps)
    cases = EXPRList{ElseCase}()
    while kind(nt(ps)) == ELSE
        kw = take_kw(ps, ELSE)
        if kind(nt(ps)) == SEMICOLON
            stmt = take(ps, SEMICOLON)
        elseif kind(nt(ps)) == IF
            stmt = parse_if(parse_statement, ps)
        else
            stmt = parse_statement(ps)
        end
        push!(cases, EXPR(ElseCase(kw, stmt)))
    end
    return EXPR(AnalogConditionalBlock(aif, cases))
end

function parse_seq_block(parse_statement, ps)
    kw = take_kw(ps, BEGIN)
    block_decl = nothing
    if kind(nt(ps)) == COLON
        colon = take(ps, COLON)
        id = accept_identifier(ps)
        decls = parse_analog_declarations(ps)
        block_decl = EXPR(AnalogBlockDecl(colon, id, decls))
    end
    stmts = EXPRList{Any}()
    while !(kind(nt(ps)) in (END, ENDMARKER,
            # These are parse errors
            ELSE))
        push!(stmts, parse_statement(ps))
    end
    return EXPR(AnalogSeqBlock(kw, block_decl, stmts, accept_kw(ps, END)))
end

function parse_reference(ps)
    # branch_reference or analog_net_reference
    return accept_identifier(ps)
    # XXX: Many more to do here
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

# id has been accepted
function parse_bpfc_function_call(ps, id)
    lparen = accept(ps, LPAREN)
    # branch_probe_function_call
    references = EXPRList{ReferenceListItem}()
    comma = nothing
    if kind(nt(ps)) != RPAREN
        parse_comma_list!(parse_reference, ps, references)
    end
    return EXPR(BPFC(id, lparen, references, accept(ps, RPAREN)))
end

function parse_function_call(parse_expression, ps, id)
    lparen = accept(ps, LPAREN)
    attrs = maybe_parse_attributes(ps)
    # branch_probe_function_call
    args = EXPRList{ListItem{EXPR}}()
    comma = nothing
    if kind(nt(ps)) != RPAREN
        parse_comma_list!(parse_expression, ps, args)
    end
    return EXPR(FunctionCall(id, attrs, lparen, args, accept(ps, RPAREN)))
end

function fc_to_bpfc(e::EXPR{FunctionCall})
    lparen = e.attrs !== nothing ? transmute!(Error(UnexpectedToken, LPAREN, LATTR), e.attrs, e.lparen) : e.lparen
    references = EXPRList{ReferenceListItem}()
    for ref in e.args
        item = ref.item
        if !isa(item, EXPR{IdentifierPrimary}) || !isa(item.id, EXPR{Identifier}) || item.range !== nothing
            item = transmute!(Error(UnexpectedToken, IDENTIFIER, nothing), item)
        else
            item = item.id
        end
        push!(references, EXPR(ReferenceListItem(ref.comma, item)))
    end
    return EXPR(BPFC(e.id, lparen, references, e.rparen))
end

function parse_lvalue(ps)
    # branch_lvalue or analog_variable_lvalue
    if kind(nt(ps)) == SYSTEM_IDENTIFIER
    elseif kind(nt(ps)) == IDENTIFIER
        if kind(nt(ps, 2)) == LPAREN
            # Expect a function call here
            return parse_analog_expression(ps)
        end
    end
    return nothing
end

function parse_analog_assignment(ps)
    lvalue = parse_lvalue(ps)
    if isa(lvalue, EXPR{FunctionCall})
        if kind(nt(ps)) == SEMICOLON
            # This is probaly a procedural assignment with a missing LHS.
            # Parse it as such.
            # TODO: We'd kinda like to steam the whitespace from the first token here and put it on the
            # error, but that's a bit complicated and requires rebuilding the tree.
            err = EXPR(UInt32(0), UInt32(0), UInt32(0), Error(MissingAssignment, nothing, nothing))
            return EXPR(AnalogProceduralAssignment(
                EXPR(AnalogVariableAssignment(err, EXPRList{ArraySpec}(), err, lvalue)),
                accept(ps, SEMICOLON)))
        end
        lvalue = fc_to_bpfc(lvalue)
        cassign = accept(ps, CASSIGN)
        # contribution_statement
        expr = parse_analog_expression(ps)
        return EXPR(ContributionStatement(lvalue, cassign,
            expr, accept(ps, SEMICOLON)))
    elseif lvalue !== nothing
        error("Unexpected non-functioncall")
    else
        return parse_analog_procedural_assignment(ps)
    end
end

function parse_analog_function_assignment(ps)
    return parse_analog_procedural_assignment(ps)
end

function parse_analog_loop_statement(parse_statement, ps)
    return @case kind(nt(ps)) begin
        FOR => parse_analog_for_loop(parse_statement, ps)
        WHILE => parse_analog_while_loop(parse_statement, ps)
        REPEAT => parse_analog_repeat_loop(parse_statement, ps)
        _ => error("Unsupported loop type: $(kw_kind)")
    end
end

function parse_analog_for_loop(parse_statement, ps)
    kw = take_kw(ps, FOR)
    lparen = accept(ps, LPAREN)
    init_stmt = parse_analog_variable_assignment(ps)
    semi_1 = accept(ps, SEMICOLON)
    cond_expr = parse_analog_expression(ps)
    semi_2 = accept(ps, SEMICOLON)
    update_stmt = parse_analog_variable_assignment(ps)
    rparen = accept(ps, RPAREN)
    if kind(nt(ps)) == SEMICOLON
        stmt = take(ps, SEMICOLON)
    else
        stmt = parse_statement(ps)
    end
    return EXPR(AnalogFor(kw, lparen, init_stmt, semi_1, cond_expr, semi_2, update_stmt, rparen, stmt))
end

function parse_analog_while_loop(parse_statement, ps)
    kw = take_kw(ps, WHILE)
    lparen = accept(ps, LPAREN)
    cond_expr = parse_analog_expression(ps)
    rparen = accept(ps, RPAREN)
    if kind(nt(ps)) == SEMICOLON
        stmt = take(ps, SEMICOLON)
    else
        stmt = parse_statement(ps)
    end
    return EXPR(AnalogWhile(kw, lparen, cond_expr, rparen, stmt))
end

function parse_analog_repeat_loop(parse_statement, ps)
    kw = take_kw(ps, REPEAT)
    lparen = accept(ps, LPAREN)
    num_repeat = accept(ps, INTEGER)
    rparen = accept(ps, RPAREN)
    if kind(nt(ps)) == SEMICOLON
        stmt = take(ps, SEMICOLON)
    else
        stmt = parse_statement(ps)
    end
    return EXPR(AnalogRepeat(kw, lparen, num_repeat, rparen, stmt))
end

function parse_analog_case_statement(parse_statement, ps)
    kw = take_kw(ps, (CASE, CASEX, CASEZ))
    lparen = accept(ps, LPAREN)
    expr = parse_analog_expression(ps)
    rparen = accept(ps, RPAREN)
    items = EXPRList{CaseItem}()
    colon = nothing
    while !(kind(nt(ps)) in (ENDCASE, ENDMARKER))
        if kind(nt(ps)) == DEFAULT
            conds = take_kw(ps, DEFAULT)
            if kind(nt(ps)) == COLON
                colon = take(ps, COLON)
            end
        else
            conds = EXPRList{ListItem{EXPR}}()
            parse_comma_list!(parse_analog_expression, ps, conds)
            colon = accept(ps, COLON)
        end
        if kind(nt(ps)) == SEMICOLON
            item = take(ps, SEMICOLON)
        else
            item = parse_statement(ps)
        end
        push!(items, EXPR(CaseItem(conds, colon, item)))
    end
    endkw = accept_kw(ps, ENDCASE)
    return EXPR(CaseStatement(kw, lparen, expr, rparen, items, endkw))
end

function parse_statement(parse_assignment, parse_statement, ps)
    attrs = maybe_parse_attributes(ps)
    item = @case kind(nt(ps)) begin
        (CASE | CASEX | CASEZ) => parse_analog_case_statement(parse_statement, ps)
        IF => parse_analog_conditional_block(parse_statement, ps)
        (REPEAT | WHILE | FOR) => parse_analog_loop_statement(parse_statement, ps)
        BEGIN => parse_seq_block(parse_statement, ps)
        INITIAL => parse_intial_block(parse_statement, ps)
        SYSTEM_IDENTIFIER => begin
            task = take_system_identifier(ps)
            if kind(nt(ps)) == LPAREN
                task = parse_function_call(parse_analog_expression, ps, task)
            end
            semi = accept(ps, SEMICOLON)
            EXPR(AnalogSystemTaskEnable(task, semi))
        end
        _ => parse_assignment(ps)
    end
    EXPR(AnalogStatement(attrs, item))
end

parse_analog_statement(ps) =
    parse_statement(parse_analog_assignment, parse_analog_statement, ps)

parse_analog_function_statement(ps) =
    parse_statement(parse_analog_function_assignment, parse_analog_function_statement, ps)

const SVA_ASSIGNMENT_OPS = (PLUS_EQ, MINUS_EQ, STAR_EQ, SLASH_EQ, STAR_STAR_EQ, PERCENT_EQ,
    AND_EQ, OR_EQ, XOR_EQ, LBITSHIFT_EQ, RBITSHIFT_EQ, LBITSHIFT_A_EQ, RBITSHIFT_A_EQ)

is_sva_eq(kind) = kind in SVA_ASSIGNMENT_OPS

function parse_analog_variable_assignment(ps)
    aspecs = EXPRList{ArraySpec}()
    lvalue = accept_identifier(ps)
    while kind(nt(ps)) == LSQUARE
        lsq = take(ps, LSQUARE)
        expr = parse_analog_expression(ps)
        rsq = take(ps, RSQUARE)
        push!(aspecs, EXPR(ArraySpec(lsq, expr, rsq)))
    end
    if kind(nt(ps)) in (EQ, SVA_ASSIGNMENT_OPS...)
        eq = take_assignment_op(ps)
        if kind(nt(ps)) == AP_LBRACE
            error("TODO")
        end
        expr = parse_analog_expression(ps)
    elseif isa(lvalue.id, EXPR{Error})
        eq = EXPR(UInt32(0), UInt32(0), UInt32(0), Error(MissingAssignment, nothing, nothing))
        expr = parse_analog_expression(ps)
    else
        # Assume that the user forgot an assignment. Parse the whole thing as an expression and emit an error.
        err = EXPR(UInt32(0), UInt32(0), UInt32(0), Error(MissingAssignment, nothing, nothing))
        if !isempty(aspecs)
            aspecs = transmute!(err, lvalue, aspecs)
            expr = parse_analog_expression(ps)
        else
            expr = parse_analog_expression(ps, lvalue)
        end
        lvalue = eq = err
    end
    return EXPR(AnalogVariableAssignment(lvalue, aspecs, eq, expr))
end

function parse_analog_procedural_assignment(ps)
    arg = parse_analog_variable_assignment(ps)
    return EXPR(AnalogProceduralAssignment(arg, accept(ps, SEMICOLON)))
end

function parse_analog_declarations(ps)
    decl_items = EXPRList{AnalogDeclarationItem}()
    while true
        attrs = maybe_parse_attributes(ps)
        item = @case kind(nt(ps)) begin
            (INPUT | OUTPUT | INOUT) => begin
                attrs === nothing || error(ps, UnexpectedAttributes)
                parse_inout_declaration(ps)
            end
            PARAMETER => parse_parameter_declaration(ps)
            (INTEGER | REAL) => parse_intreal_declaration(ps)
            ANALOG => error!(ps, UnexpectedToken, nothing, ANALOG, true)
            _ => break
        end
        semi = accept(ps, SEMICOLON)
        push!(decl_items, EXPR(AnalogDeclarationItem(attrs, item, semi)))
    end
    return decl_items
end

function parse_analog_function_decl(ps, akw)
    fkw = take_kw(ps, FUNCTION)
    ftkw = nothing
    if kind(nt(ps)) in (INTEGER, REAL)
        ftkw = take_kw(ps, (INTEGER, REAL))
    end
    id = accept_identifier(ps)
    semi = accept(ps, SEMICOLON)
    decl_items = parse_analog_declarations(ps)
    stmt = parse_analog_function_statement(ps)
    endkw = accept_end(ps, ENDFUNCTION)
    return EXPR(AnalogFunctionDeclaration(akw, fkw, ftkw, id, semi, decl_items, stmt, endkw))
end

function parse_analog_block(ps)
    kw = take_kw(ps, ANALOG)
    if kind(nt(ps)) == INITIAL
        return parse_analog_function_statement(ps)
    elseif kind(nt(ps)) == FUNCTION
        return parse_analog_function_decl(ps, kw)
    else
        return EXPR(AnalogBlock(kw, parse_analog_statement(ps)))
    end
end

function parse_branch_declaration(ps)
    kw = take_kw(ps, BRANCH)
    lparen = accept(ps, LPAREN)
    # TODO: Also ports, hierarchies
    references = EXPRList{ReferenceListItem}()
    if kind(nt(ps)) != RPAREN
        parse_comma_list!(parse_reference, ps, references)
    end
    rparen = accept(ps, RPAREN)
    ids = EXPRList{ListItem{EXPR{BranchIdentifier}}}()
    comma = nothing
    while true
        branch_id = accept_identifier(ps)
        rang = nothing
        if kind(nt(ps)) == LSQUARE
            rang = parse_range(ps)
        end
        push!(ids, EXPR(ListItem{EXPR{BranchIdentifier}}(comma, EXPR(BranchIdentifier(branch_id, rang)))))
        kind(nt(ps)) == COMMA || break
        comma = take(ps, COMMA)
    end
    return EXPR(BranchDeclaration(kw, lparen, references, rparen, ids))
end

function parse_macro_error!(ps)
    ps.errored = true
    args = EXPRList{Error}()
    # TODO: This is wrong in corner cases (e.g. when the macro got undefined just after this or when we're in an expansion)
    macrodef = ps.macros[Symbol(resolve_identifier(ps, active_file(ps), nt(ps))[2:end])]
    while true
        ntk = kind(nt(ps))
        ntk == ENDMARKER && break
        push!(args, EXPR!(Error(ntk, nothing, nothing), ps))
        ntk == RPAREN && break
    end
    return EXPR(MacroError(BadMacroCall, macrodef, args))
end

function handle_unexpected_token!(ps, expected=nothing, consume=true)
    @case kind(nt(ps)) begin
        PREPROC_ERR_BAD_MACRO_CALL => parse_macro_error!(ps)
        PREPROC_ERR_UNDEF_MACRO => error!(ps, UndefinedMacro)
        PREPROC_ERR_RECURSIVE_MACRO => error!(ps, RecursiveMacro)
        _ => error!(ps, UnexpectedToken, expected, kind(nt(ps)), consume)
    end
end

function parse_module_items(ps)
    items = EXPRList{ModuleItem}()
    while !(kind(nt(ps)) in (ENDMODULE, ENDMARKER))
        attrs = maybe_parse_attributes(ps)
        (item, semi) = @case kind(nt(ps)) begin
            INPUT | OUTPUT | INOUT => (parse_inout_declaration(ps), accept(ps, SEMICOLON))
            PARAMETER => (parse_parameter_declaration(ps), accept(ps, SEMICOLON))
            ANALOG => (parse_analog_block(ps), nothing)
            ALIASPARAM => (parse_aliasparameter_declaration(ps), accept(ps, SEMICOLON))
            # Identifiers interpreted as discipline identifier
            IDENTIFIER => (parse_net_declaration(ps), nothing)
            BRANCH => (parse_branch_declaration(ps), accept(ps, SEMICOLON))
            (INTEGER | REAL) => (parse_intreal_declaration(ps), accept(ps, SEMICOLON))
            _ => (handle_unexpected_token!(ps), nothing)
        end
        push!(items, EXPR(ModuleItem(attrs, item, semi)))
    end
    return items, accept_end(ps, ENDMODULE)
end

function parse_ports(ps)
    kind(nt(ps)) == SEMICOLON && return nothing

    lparen = @lift accept(ps, LPAREN)
    declaration_list = false
    @case kind(nt(ps)) begin
        RPAREN => return (EXPR(PortDeclarations(lparen, take(ps, RPAREN))), true)
        INPUT | OUTPUT | INOUT => (declaration_list = true)
        IDENTIFIER => nothing
        _ => return error!(ps, UnexpectedToken)
    end

    ports = declaration_list ?
        EXPRList{PortDeclListItem}() :
        EXPRList{IdentifierListItem}()
    comma = nothing
    while true
        if declaration_list
            push!(ports, EXPR(PortDeclListItem(comma, @lift parse_inout_declaration(ps))))
        else
            if kind(nt(ps)) in (INPUT, OUTPUT, INOUT)
                return error(ps, "Cannot mix port names and port declarations")
            end
            push!(ports, EXPR(IdentifierListItem(comma, @lift accept_identifier(ps))))
        end
        kind(nt(ps)) == RPAREN && break
        kind(nt(ps)) == COMMA || break
        comma = take(ps, COMMA)
    end
    return EXPR(PortDeclarations(lparen, ports, accept(ps, RPAREN)))
end

function parse_module(ps)
    kw = take_kw(ps)
    id = @lift accept_identifier(ps)
    mp_port_list = nothing
    if kind(nt(ps)) == HASH
        mp_port_list = @lift parse_module_parameter_port_list(ps)
    end
    port_list = @lift parse_ports(ps)
    semi = @lift accept_noconsume(ps, SEMICOLON)
    items, endkw = parse_module_items(ps)
    EXPR(VerilogModule(kw, id, mp_port_list, port_list, semi, items, endkw))
end

function parse_config(ps)
    error("TODO: VerilogA parse_config unimplemented")
end

function parse_primitive(ps)
    error("TODO: VerilogA parse_primitive unimplemented")
end

function parse_paramset(ps)
    error("TODO: VerilogA parse_paramset unimplemented")
end

function parse_connectrules(ps)
    error("TODO: VerilogA parse_connectrules unimplemented")
end

function parse_discipline_items(ps)
    items = EXPRList{DisciplineItem}()
    while kind(nt(ps)) != ENDDISCIPLINE
        item = @case kind(nt(ps)) begin
            POTENTIAL | FLOW => begin
                kw = take_kw(ps)
                if kind(nt(ps)) == DOT
                    error("TODO")
                end
                EXPR(NatureBinding(kw, accept_identifier(ps)))
            end
            DOMAIN => begin
                kw = take_kw(ps)
                EXPR(DomainBinding(kw, accept_kw(ps, (DISCRETE, CONTINUOUS))))
            end
            _ => error(ps, UnexpectedToken)
        end
        semi = accept(ps, SEMICOLON)
        push!(items, EXPR(DisciplineItem(item, semi)))
    end
    items, take_kw(ps, ENDDISCIPLINE)
end

function parse_discipline(ps)
    kw = take_kw(ps)
    id = accept_identifier(ps)
    semi = nothing
    if kind(nt(ps)) == SEMICOLON
        semi = take(ps, SEMICOLON)
    end
    items, endkw = parse_discipline_items(ps)
    EXPR(DisciplineDeclaration(kw, id, semi, items, endkw))
end

function parse_nature_items(ps)
    items = EXPRList{NatureItem}()
    while kind(nt(ps)) != ENDNATURE
        if isident(kind(nt(ps)))
            id = accept_identifier(ps)
        else
            id = accept_kw(ps, (ABSTOL, ACCESS, DDT_NATURE, IDT_NATURE, UNITS))
        end
        eq = accept(ps, EQ)
        expr = parse_constant_expression(ps)
        semi = accept(ps, SEMICOLON)
        push!(items, EXPR(NatureItem(id, eq, expr, semi)))
    end
    items, take_kw(ps, ENDNATURE)
end

function parse_nature(ps)
    kw = take_kw(ps)
    id = accept_identifier(ps)
    parent = nothing
    if kind(nt(ps)) == COLON
        error("TODO")
    end
    semi = nothing
    if kind(nt(ps)) == SEMICOLON
        semi = take(ps, SEMICOLON)
    end
    items, endkw = parse_nature_items(ps)
    EXPR(NatureDeclaration(kw, id, nothing, semi, items, endkw))
end
