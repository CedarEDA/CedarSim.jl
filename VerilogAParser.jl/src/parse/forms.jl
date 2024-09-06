struct Keyword <: Terminal
    kw::Kind
end
struct Operator <: Terminal
    op::Kind
end
struct IdentifierPart <: Terminal
    escaped::Bool
end
struct SystemIdentifier <: Terminal; end
struct Notation <: Terminal; end
struct BuiltinFunc <: Terminal; end
struct Literal <: Terminal; end
struct StringLiteral <: Terminal; end
struct FloatLiteral <: Terminal; end

@enum(ErrorKind,
    UnexpectedToken,
    MissingAssignment,
    ExpectedMacro,
    UndefinedMacro,
    RecursiveMacro,
    MissingMacro,
    BadMacroCall,
    IncludeDirectiveJunk)

struct Error <: Terminal
    kind::Union{ErrorKind, Kind}
    context
    got
end

struct MacroError
    kind::ErrorKind
    def::MacroDef
    args::EXPRList{Error}
end
allchildren(me::MacroError) = me.args

const Maybe{T} = Union{T, Nothing}
const EXPRErr{T} = Union{EXPR{T}, EXPR{Error}, EXPR{MacroError}}

struct ListItem{T}
    comma::Maybe{EXPR{Notation}}
    item::T
end
allchildren(p::ListItem) = (p.comma, p.item)

struct IdentifierConcatItem
    bb::EXPR{Notation}
    id::EXPRErr{IdentifierPart}
end
allchildren(ici::IdentifierConcatItem) = (ici.bb, ici.id)

struct Identifier
    id::EXPRErr{IdentifierPart}
    concat::Maybe{EXPRList{IdentifierConcatItem}}
end
allchildren(p::Identifier) = p.concat === nothing ? (p.id,) : (p.id, p.concat...)

const IdentifierListItem = ListItem{EXPR{Identifier}}

struct AttrSpec
    name::Union{EXPR{Identifier}, EXPR{Keyword}, EXPR{Error}, EXPR{MacroError}}
    eq::EXPRErr{Notation}
    val::EXPRErr
end
allchildren(p::AttrSpec) = (p.name, p.eq, p.val)

struct Attributes
    open::EXPR{Notation}
    specs::EXPRList{ListItem{EXPR{AttrSpec}}}
    close::EXPRErr{Notation}
end
allchildren(p::Attributes) = (p.open, p.specs..., p.close)

struct InOutDeclaration
    kw::EXPR{Keyword}
    discipline::Maybe{EXPR{Identifier}}
    net_type::Maybe{EXPR{Keyword}}
    signed::Maybe{EXPRErr{Keyword}}
    range::Maybe{EXPR}
    portnames::EXPRList{IdentifierListItem}
end
allchildren(p::InOutDeclaration) = (p.kw, p.discipline, p.net_type, p.range, p.portnames...)

const PortDeclListItem = ListItem{EXPR{InOutDeclaration}}

struct PortDeclarations
    lparen::EXPR{Notation}
    ports::Union{EXPRList{IdentifierListItem}, EXPRList{PortDeclListItem}}
    rparen::EXPR{Notation}
end
PortDeclarations(lparen, rparen) = PortDeclarations(lparen, Vector{EXPR{IdentifierListItem}}(), rparen)
allchildren(p::PortDeclarations) = (p.lparen, p.ports..., p.rparen)

const NetNameListItem = ListItem{EXPR{Identifier}}
const NetAssignmentListItem = ListItem{EXPR} # XXX

struct NetDeclaration
    net_type::Maybe{EXPR{Keyword}}
    discipline::Maybe{EXPR{Identifier}}
    # XXX: More fields
    net_names::Union{EXPRList{NetNameListItem}, EXPRList{NetAssignmentListItem}}
    semi::EXPRErr{Notation}
end
allchildren(n::NetDeclaration) = (n.net_type, n.discipline, n.net_names..., n.semi)

struct RangeSpec
    lsquare::EXPR{Notation}
    low::EXPR
    colon::EXPRErr{Notation}
    high::EXPR
    rsquare::EXPRErr{Notation}
end
allchildren(r::RangeSpec) = (r.lsquare, r.low, r.colon, r.high, r.rsquare)


struct ValueRange
    vtype::EXPR{Keyword}
    r::EXPR
end
allchildren(v::ValueRange) = (v.vtype, v.r)

struct ParamAssignment
    id::EXPR{Identifier}
    prange::Maybe{EXPR}
    eq::EXPRErr{Notation}
    default_expr::EXPR
    value_range::Maybe{EXPRList{ValueRange}}
end
allchildren(pa::ParamAssignment) = (pa.id, pa.prange, pa.eq, pa.default_expr, something(pa.value_range, (nothing,))...)

const ParamAssignmentListItem = ListItem{EXPR{ParamAssignment}}

struct ParameterDeclaration
    kw::EXPR{Keyword}
    ptype::Maybe{EXPR{Keyword}}
    range::Maybe{EXPR}
    params::EXPRList{ParamAssignmentListItem}
end
allchildren(pd::ParameterDeclaration) = (pd.kw, pd.ptype, pd.range, pd.params...)

const ReferenceListItem = ListItem{EXPRErr{Identifier}}

struct AliasParameterDeclaration
    kw::EXPR{Keyword}
    id::EXPR{Identifier}
    eq::EXPR{Notation}
    value::EXPR{Identifier}
end
allchildren(pd::AliasParameterDeclaration) = (pd.kw, pd.id, pd.eq, pd.value)

# branch_probe_function_call
struct BPFC
    id::EXPR{Identifier}
    lparen::EXPR{Notation}
    references::EXPRList{ReferenceListItem}
    rparen::EXPRErr{Notation}
end
allchildren(bpfc::BPFC) = (bpfc.id, bpfc.lparen, bpfc.references..., bpfc.rparen)

# Some other kind of function call other than BPFC
struct FunctionCall
    id::Union{EXPR{Identifier}, EXPR{BuiltinFunc}, EXPR{SystemIdentifier}}
    attrs::Maybe{Attributes}
    lparen::EXPR{Notation}
    args::EXPRList{ListItem{EXPR}}
    rparen::EXPRErr{Notation}
end
allchildren(fc::FunctionCall) = (fc.id, fc.attrs, fc.lparen, fc.args..., fc.rparen)

struct ContributionStatement
    lvalue::EXPR
    cassign::EXPRErr{Notation}
    assign_expr::EXPRErr
    semi::EXPRErr{Notation}
end
allchildren(cs::ContributionStatement) = (cs.lvalue, cs.cassign, cs.assign_expr, cs.semi)

struct IdentifierPrimary
    id::Union{EXPR{Identifier}, EXPR{SystemIdentifier}}
    range::Maybe{EXPR}
end
allchildren(ip::IdentifierPrimary) = (ip.id, ip.range)

struct BasedInteger
    size::Maybe{EXPR{Literal}}
    base_spec::Maybe{EXPR{Notation}}
    # N.B.: Due to a lex ambiguity, the corresponding token may be IDENTIFIER
    digs::EXPR{Literal}
end
allchildren(bi::BasedInteger) = (bi.size, bi.base_spec, bi.digs)

struct BinaryExpression
    lhs::EXPR
    op::EXPR{Operator}
    attrs::Maybe{Attributes}
    rhs::EXPR
end
allchildren(be::BinaryExpression) = (be.lhs, be.op, be.attrs, be.rhs)

struct UnaryOp
    op::EXPR{Operator}
    attrs::Maybe{Attributes}
    operand::EXPR
end
allchildren(be::UnaryOp) = (be.op, be.attrs, be.operand)

struct AnalogBlock
    kw::EXPR{Keyword}
    stmt::EXPR
end
allchildren(ab::AnalogBlock) = (ab.kw, ab.stmt)

struct ModuleItem
    attrs::Maybe{EXPR{Attributes}}
    item::EXPR
    semi::Maybe{EXPRErr{Notation}}
end
allchildren(mi::ModuleItem) = (mi.attrs, mi.item, mi.semi)

struct VerilogModule
    kw::EXPR{Keyword}
    id::EXPR{Identifier}
    mp_port_list::Nothing
    port_list::Maybe{EXPRErr{PortDeclarations}}
    semi::EXPRErr{Notation}
    items::EXPRList{ModuleItem}
    endkw::EXPRErr{Keyword}
end
allchildren(vm::VerilogModule) = (vm.kw, vm.id, vm.mp_port_list, vm.port_list, vm.semi, vm.items..., vm.endkw)

struct VerilogSource
    stmts::EXPRList{Any}
end
allchildren(vs::VerilogSource) = vs.stmts

struct DomainBinding
    domain::EXPR{Keyword}
    disc_or_cont::EXPR{Keyword}
end
allchildren(db::DomainBinding) = (db.domain, db.disc_or_cont)

struct DisciplineItem
    item::EXPR
    semi::EXPR{Notation}
end
allchildren(di::DisciplineItem) = (di.item, di.semi)

struct DisciplineDeclaration
    kw::EXPR{Keyword}
    id::EXPR{Identifier}
    semi::EXPR{Notation}
    items::EXPRList{DisciplineItem}
    endkw::EXPR{Keyword}
end
allchildren(dd::DisciplineDeclaration) = (dd.kw, dd.id, dd.semi, dd.items..., dd.endkw)

struct DisciplineNatureRef
    discipline::EXPR{Identifier}
    dot::EXPR{Notation}
    nature::EXPR{Identifier}
end
allchildren(dnr::DisciplineNatureRef) = (dnr.discipline, dnr.dot, dnr.nature)

struct ParentNatureSpec
    colon::EXPR{Notation}
    parent::Union{EXPR{Identifier}, EXPR{DisciplineNatureRef}}
end
allchildren(pn::ParentNatureSpec) = (pn.colon, pn.parent)

struct NatureItem
    attribute::Union{EXPR{Keyword}, EXPR{Identifier}}
    eq::EXPR{Notation}
    value::EXPR
    semi::EXPR{Notation}
end
allchildren(ni::NatureItem) = (ni.attribute, ni.eq, ni.value, ni.semi)

struct NatureDeclaration
    kw::EXPR{Keyword}
    id::EXPR{Identifier}
    parent::Maybe{EXPR{ParentNatureSpec}}
    semi::EXPR{Notation}
    items::EXPRList{NatureItem}
    endkw::EXPR{Keyword}
end
allchildren(dd::NatureDeclaration) = (dd.kw, dd.id, dd.parent, dd.semi, dd.items..., dd.endkw)

struct NatureBinding
    kw::EXPR{Keyword}
    id::EXPR{Identifier}
end
allchildren(nb::NatureBinding) = (nb.kw, nb.id)

struct AnalogDeclarationItem
    attrs::Maybe{EXPR{Attributes}}
    item::EXPR
    semi::EXPRErr{Notation}
end
allchildren(fdi::AnalogDeclarationItem) = (fdi.attrs, fdi.item, fdi.semi)

struct AnalogStatement
    attrs::Maybe{EXPR{Attributes}}
    stmt::EXPR
end
allchildren(afs::AnalogStatement) = (afs.attrs, afs.stmt)

struct AnalogFunctionDeclaration
    akw::EXPR{Keyword}
    fkw::EXPR{Keyword}
    fty::Maybe{EXPR{Keyword}}
    id::EXPR{Identifier}
    semi::EXPRErr{Notation}
    items::EXPRList{AnalogDeclarationItem}
    stmt::EXPR{AnalogStatement}
    endkw::EXPRErr{Keyword}
end
allchildren(fd::AnalogFunctionDeclaration) = (fd.akw, fd.fkw, fd.fty, fd.id, fd.semi, fd.items..., fd.stmt, fd.endkw)

struct IntRealDeclaration
    kw::EXPR{Keyword}
    idents::EXPRList{IdentifierListItem}
end
allchildren(ird::IntRealDeclaration) = (ird.kw, ird.idents...)

struct AnalogBlockDecl
    colon::EXPR{Notation}
    id::EXPR{Identifier}
    decls::EXPRList{AnalogDeclarationItem}
end
allchildren(abd::AnalogBlockDecl) = (abd.colon, abd.id, abd.decls...)

# AnalogSeqBlock(kw, block_decl, stmts, take_kw(ps, END))
struct AnalogSeqBlock
    kwbegin::EXPR{Keyword}
    decl::Maybe{EXPR{AnalogBlockDecl}}
    stmts::EXPRList{Any}
    kwend::EXPRErr{Keyword}
end
allchildren(asb::AnalogSeqBlock) = (asb.kwbegin, asb.decl, asb.stmts..., asb.kwend)

struct AnalogIf
    kw::EXPR{Keyword}
    lparen::EXPRErr{Notation}
    condition::EXPR
    rparen::EXPRErr{Notation}
    stmt::EXPR
end
allchildren(ai::AnalogIf) = (ai.kw, ai.lparen, ai.condition, ai.rparen, ai.stmt)

struct AnalogFor
    kw::EXPR{Keyword}
    lparen::EXPRErr{Notation}
    init_stmt::EXPR
    semi1::EXPRErr{Notation}
    cond_expr::EXPR
    semi2::EXPRErr{Notation}
    update_stmt::EXPR
    rparen::EXPRErr{Notation}
    stmt::EXPR
end
allchildren(af::AnalogFor) = (af.kw, af.lparen, af.init_stmt, af.semi1, af.cond_expr, af.semi2, af.update_stmt, af.rparen, af.stmt)
struct AnalogWhile
    kw::EXPR{Keyword}
    lparen::EXPRErr{Notation}
    cond_expr::EXPR
    rparen::EXPRErr{Notation}
    stmt::EXPR
end
allchildren(af::AnalogWhile) = (af.kw, af.lparen, af.cond_expr, af.rparen, af.stmt)

struct AnalogRepeat
    kw::EXPR{Keyword}
    lparen::EXPRErr{Notation}
    num_repeat::EXPR{Literal}
    rparen::EXPRErr{Notation}
    stmt::EXPR
end
allchildren(af::AnalogRepeat) = (af.kw, af.lparen, af.num_repeat, af.rparen, af.stmt)

struct ElseCase
    kw::EXPR{Keyword}
    stmt::EXPR
end
allchildren(ec::ElseCase) = (ec.kw, ec.stmt)

struct AnalogConditionalBlock
    aif::EXPR{AnalogIf}
    elsecases::EXPRList{ElseCase}
end
allchildren(acb::AnalogConditionalBlock) = (acb.aif, acb.elsecases...)

struct ArraySpec
    lsq::EXPR{Notation}
    arr::EXPR
    rsq::EXPRErr{Notation}
end
allchildren(as::ArraySpec) = (as.lsq, as.arr, as.rsq)

struct AnalogVariableAssignment
    lvalue::EXPRErr{Identifier}
    aspecs::EXPRList{ArraySpec}
    eq::EXPRErr{Operator}
    rvalue::EXPRErr
end
allchildren(aa::AnalogVariableAssignment) = (aa.lvalue, aa.aspecs..., aa.eq, aa.rvalue)

struct AnalogProceduralAssignment
    assign::EXPR{AnalogVariableAssignment}
    semi::EXPRErr{Notation}
end
allchildren(aa::AnalogProceduralAssignment) = (aa.assign, aa.semi)

struct Parens
    lparen::EXPR{Notation}
    inner::EXPRErr
    rparen::EXPRErr{Notation}
end
allchildren(p::Parens) = (p.lparen, p.inner, p.rparen)

struct BranchIdentifier
    id::EXPR{Identifier}
    rang::Maybe{EXPR}
end
allchildren(bi::BranchIdentifier) = (bi.id, bi.rang)

struct BranchDeclaration
    kw::EXPR{Keyword}
    lparen::EXPR{Notation}
    references::EXPRList{ReferenceListItem}
    rparen::EXPR{Notation}
    ids::EXPRList{ListItem{EXPR{BranchIdentifier}}}
end
allchildren(bd::BranchDeclaration) = (bd.kw, bd.lparen, bd.references..., bd.rparen, bd.ids...)

struct CaseItem
    conds::Union{EXPR{Keyword}, EXPRList{ListItem{EXPR}}}
    colon::EXPRErr{Notation}
    item::EXPR
end
allchildren(ci::CaseItem) = ((isa(ci.conds, EXPRList) ? ci.conds : (ci.conds,))..., ci.colon, ci.item)

struct CaseStatement
    kw::EXPR{Keyword}
    lparen::EXPR{Notation}
    switch::EXPR
    rparen::EXPR{Notation}
    cases::EXPRList{CaseItem}
    endkw::EXPRErr{Keyword}
end
allchildren(cs::CaseStatement) = (cs.kw, cs.lparen, cs.switch, cs.rparen, cs.cases..., cs.endkw)

struct AnalogSystemTaskEnable
    task::Union{EXPR{SystemIdentifier}, EXPR{FunctionCall}}
    semi::EXPRErr{Notation}
end
allchildren(ste::AnalogSystemTaskEnable) = (ste.task, ste.semi)

struct TernaryExpr
    condition::EXPR
    questionmark::EXPR{Notation}
    ifcase::EXPR
    colon::EXPRErr{Notation}
    elsecase::EXPR
end
allchildren(te::TernaryExpr) = (te.condition, te.questionmark, te.ifcase, te.colon, te.elsecase)

function EXPR!(val::Terminal, ps)
    vp = ps.nvirtpos[1]
    next(ps)
    vp′ = ps.nvirtpos[1]
    EXPR(vp′[1] - vp[1], vp[2] - vp[1], UInt32(ps.t.endbyte - ps.t.startbyte + 1), val)
end

function transmute!(newform, exprs...)
    fullwidth = sum(x->x.fullwidth, exprs) % UInt32
    EXPR(fullwidth, exprs[1].off, UInt32(fullwidth - (exprs[end].fullwidth - (exprs[end].off + exprs[end].width))), newform)
end

function EXPR(val)
    firstchild = nothing
    lastchild = nothing
    fw::UInt32 = 0
    for child in allchildren(val)
        child === nothing && continue
        if firstchild === nothing
            firstchild = child
        end
        lastchild = child
        fw += child.fullwidth
    end
    # TODO: may need better invariants upstream
    #if isnothing(firstchild)
    #    return EXPR(fw, fw, fw,0, val)
    #end
    EXPR(fw, firstchild.off, fw - firstchild.off - (lastchild.fullwidth - (lastchild.width+lastchild.off)), val)
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
