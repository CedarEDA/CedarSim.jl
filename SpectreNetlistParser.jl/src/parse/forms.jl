import ..AbstractTerminal, ..isunit
abstract type Terminal <: AbstractTerminal end
EXPRS.allchildren(::Terminal) = ()

struct Keyword <: Terminal
    kw::Kind
end
struct Operator <: Terminal
    op::Kind
end
struct Identifier <: Terminal end
struct SystemIdentifier <: Terminal; end
struct Notation <: Terminal; end
struct BuiltinFunc <: Terminal; end
struct BuiltinConst <: Terminal; end
struct Literal <: Terminal; end
struct StringLiteral <: Terminal; end
struct FloatLiteral <: Terminal; end
struct IntLiteral <: Terminal; end
struct UnitLiteral <: Terminal; end
isunit(::UnitLiteral) = true

@enum(ErrorKind,
    UnexpectedToken,
)

struct Error <: Terminal
    kind::ErrorKind
end

const Maybe{T} = Union{T, Nothing}
const EXPRErr{T} = Union{EXPR{T}, EXPR{Error}}

abstract type AbstractASTNode end
abstract type AbstractBlockASTNode <: AbstractASTNode end

@generated function EXPRS.allchildren(x::AbstractASTNode)
    expr = Expr(:tuple)
    for i in 1:fieldcount(x)
        nam = fieldname(x, i)
        typ = fieldtype(x,i)
        if typ <: EXPRList
            push!(expr.args, Expr(:(...), :(x.$(nam))))
        else
            push!(expr.args, :(x.$nam))
        end
    end
    return expr
end

# TODO: Since the unit has to come after the number (without) space
# this breaks the property that we can put spaces between all terminals
# and have valid source code.
# Should probably introduce a single UnitfulNumericValue terminal instead
struct NumericValue <: AbstractASTNode
    val::Union{EXPR{IntLiteral}, EXPR{FloatLiteral}}
    unit::Maybe{EXPR{UnitLiteral}}
end

struct SubcktNode <: AbstractASTNode
    id::EXPR{Identifier}
    dot::EXPR{Notation}
end

struct SNode <: AbstractASTNode
    subckts::EXPRList{SubcktNode}
    node::Union{EXPR{Identifier}, EXPR{IntLiteral}}
end

###########
# Include #
###########
struct IncludeSection <: AbstractASTNode
    kw::EXPR{Keyword}
    eq::EXPR{Notation}
    section::EXPR{Identifier}
end

struct Include <: AbstractASTNode
    kw::EXPR{Keyword}
    filename::EXPR{StringLiteral}
    section::Union{Nothing, EXPR{IncludeSection}}
    nl::EXPR{Notation}
end

struct AHDLInclude <: AbstractASTNode
    kw::EXPR{Keyword}
    filename::EXPR{StringLiteral}
    nl::EXPR{Notation}
end

struct Parameter <: AbstractASTNode
    name::EXPR{Identifier}
    eq::EXPR{Notation}
    val::EXPR
end

struct SubcktNodes <: AbstractASTNode
    lparen::Maybe{EXPR{Notation}}
    nodes::Maybe{EXPRList{SNode}}
    rparen::Maybe{EXPR{Notation}}
end


struct Subckt <: AbstractBlockASTNode
    inline::Maybe{EXPR{Keyword}}
    kw_start::EXPR{Keyword}
    name::EXPR{Identifier}
    subckt_nodes::Maybe{EXPR{SubcktNodes}}
    nl1::EXPR{Notation}
    stmts::EXPRList
    kw_end::EXPR{Keyword} # ends
    end_name::Maybe{EXPR{Identifier}}
    nl2::EXPR{Notation}
end

 struct ParamTest <: AbstractASTNode
    name::EXPR{Identifier}
    kw::EXPR{Keyword}
    parameters::EXPRList{Parameter}
    nl::EXPR{Notation}
end

struct FunctionArgs{T} <: AbstractASTNode
    comma::Maybe{EXPR{Notation}}
    item::T
end

struct FunctionCall <: AbstractASTNode
    id::Union{EXPR{Identifier}, EXPR{BuiltinFunc}}
    lparen::EXPR{Notation}
    args::EXPRList{FunctionArgs{EXPR}}
    rparen::EXPRErr{Notation}
end

struct FunctionDeclArg <: AbstractASTNode
    typ::EXPR{Keyword} # always REAL
    id::EXPR{Identifier}
end
# TODO: From manual:
# > Note: If you create a user-defined function with the same name as a built-in function, the
# > Spectre simulator issues a warning and runs the user-defined function
struct FunctionDecl <: AbstractASTNode
    rtype::EXPR{Keyword} # always REAL
    id::EXPR{Identifier}
    lparen::EXPR{Notation}
    args::EXPRList{FunctionArgs{EXPR{FunctionDeclArg}}}
    rparen::EXPR{Notation}
    lbrace::EXPR{Notation}
    nl1::EXPR{Notation}
    ret::EXPR{Keyword}
    body::EXPR
    semicolon::EXPR{Notation}
    nl2::EXPR{Notation}
    rbrace::EXPR{Notation}
    nl3::EXPR{Notation}
end

struct If <: AbstractASTNode
    kw::EXPR{Keyword}
    lparen::EXPR{Notation}
    condition::EXPR
    rparen::EXPRErr{Notation}
    lbrace::EXPR{Notation}
    nl1::EXPR{Notation}
    stmt::EXPR
    rbrace::EXPR{Notation}
end

struct ElseIf <: AbstractASTNode
    kw::EXPR{Keyword}
    kw2::EXPR{Keyword}
    lparen::EXPR{Notation}
    condition::EXPR
    rparen::EXPRErr{Notation}
    lbrace::EXPR{Notation}
    nl1::EXPR{Notation}
    stmt::EXPR
    rbrace::EXPR{Notation}
end

struct Else <: AbstractASTNode
    kw::EXPR{Keyword}
    lbrace::EXPR{Notation}
    nl::EXPR{Notation}
    stmt::EXPR
    rbrace::EXPR{Notation}
end

struct ConditionalBlock <: AbstractASTNode
    aif::EXPR{If}
    elseifs::EXPRList{ElseIf}
    aelse::Maybe{EXPR{Else}}
    nl::EXPR{Notation}
end

struct TernaryExpr <: AbstractASTNode
    condition::EXPR
    questionmark::EXPR{Notation}
    ifcase::EXPR
    colon::EXPRErr{Notation}
    elsecase::EXPR
end

struct Parens <: AbstractASTNode
    lparen::EXPR{Notation}
    inner::EXPRErr
    rparen::EXPRErr{Notation}
end

##############
# Parameters #
##############
struct Parameters <: AbstractASTNode
    kw::EXPR{Keyword}
    params::EXPRList{Parameter}
    nl::EXPR{Notation}
end


#########
# Array #
#########

struct SpectreArray <: AbstractASTNode
    lbrace::EXPR{Notation}
    items::EXPRList
    rbrace::EXPR{Notation}
end

############
# Language #
############
struct Simulator <: AbstractASTNode
    kw::EXPR{Keyword}
    langkw::EXPR{Keyword}
    eq::EXPR{Notation}
    lang::EXPR{Keyword}
    params::EXPRList{Parameter} # Can only be insensitive = true/false ?
    nl::EXPR{Notation}
end

struct SNodeList <: AbstractASTNode
    lparen::Maybe{EXPR{Notation}} # TODO: Parens are optional
    nodes::EXPRList{SNode}
    rparen::Maybe{EXPR{Notation}}
end

############
# Instance #
############
struct Instance <: AbstractASTNode
    name::EXPR{Identifier}
    nodelist::EXPR{SNodeList}
    master::EXPR{Identifier}
    params::EXPRList{Parameter}
    nl::EXPR{Notation}
end

###########
# Control #
###########

struct AlterGroup <: AbstractASTNode
    name::EXPR{Identifier}
    kw::EXPR{Keyword}
    lbrace::EXPR{Notation}
    nl1::EXPR{Notation}
    exprs::EXPRList
    rbrace::EXPR{Notation}
    nl2::EXPR{Notation}
end

struct SaveSignalModifier <: AbstractASTNode
    colon::EXPR{Notation}
    modifier::Union{EXPR{Keyword}, EXPR{Identifier}, EXPR{IntLiteral}}
end

struct SaveSignal <: AbstractASTNode
    component::Maybe{EXPR{SNode}}
    modifier::Maybe{EXPR{SaveSignalModifier}}
end

struct Save <: AbstractASTNode
    kw::EXPR{Keyword}
    signals::EXPRList{SaveSignal}
    nl::EXPR{Notation}
end

struct ICParameter <: AbstractASTNode
    name::EXPR{SNode}
    eq::EXPR{Notation}
    val::EXPR
end

struct Ic <: AbstractASTNode
    kw::EXPR{Keyword}
    parameters::EXPRList{ICParameter}
    nl::EXPR{Notation}
end

struct NodeSet <: AbstractASTNode
    kw::EXPR{Keyword}
    parameters::EXPRList{Parameter}
    nl::EXPR{Notation}
end

struct Alter <: AbstractASTNode
    name::Maybe{EXPR{Identifier}}
    kw::EXPR{Keyword}
    params::EXPRList{Parameter}
    nl::EXPR{Notation}
end

struct Check <: AbstractASTNode
    name::Maybe{EXPR{Identifier}}
    kw::EXPR{Keyword}
    params::EXPRList{Parameter}
    nl::EXPR{Notation}
end

struct CheckLimit <: AbstractASTNode
    name::Maybe{EXPR{Identifier}}
    kw::EXPR{Keyword}
    params::EXPRList{Parameter}
    nl::EXPR{Notation}
end

struct Info <: AbstractASTNode
    name::Maybe{EXPR{Identifier}}
    kw::EXPR{Keyword}
    params::EXPRList{Parameter}
    nl::EXPR{Notation}
end

struct Options <: AbstractASTNode
    name::EXPR{Identifier}
    kw::EXPR{Keyword}
    params::EXPRList{Parameter}
    nl::EXPR{Notation}
end

struct Set <: AbstractASTNode
    name::EXPR{Identifier}
    kw::EXPR{Keyword}
    params::EXPRList{Parameter}
    nl::EXPR{Notation}
end

struct Shell <: AbstractASTNode
    name::EXPR{Identifier}
    kw::EXPR{Keyword}
    params::EXPRList{Parameter}
    nl::EXPR{Notation}
end


#########
# Model #
#########
struct Model <: AbstractASTNode
    kw::EXPR{Keyword}
    name::EXPR{Identifier}
    master_name::EXPR{Identifier}
    params::EXPRList{Parameter}
    nl::EXPR{Notation}
end

############
# Analysis #
############
struct Analysis <: AbstractASTNode
    name::EXPR{Identifier}
    nodelist::Maybe{EXPR{SNodeList}}
    kw::EXPR{Keyword}
    params::EXPRList{Parameter}
    nl::EXPR{Notation}
end

##########
# Global #
##########
struct Global <: AbstractASTNode
    kw::EXPR{Keyword}
    globals::EXPRList{SNode}
    nl::EXPR{Notation}
end

struct SpectreNetlistSource <: AbstractBlockASTNode
    stmts::EXPRList{Any}
end
EXPRS.allchildren(sns::SpectreNetlistSource) = sns.stmts

struct UnaryOp <: AbstractASTNode
    op::EXPR{Operator}
    operand::EXPR
end

struct BinaryExpression <: AbstractASTNode
    lhs::EXPR
    op::EXPR{Operator}
    rhs::EXPR
end

function EXPR!(val::Terminal, ps)
    prev = ps.prevpos
    fw = ps.allpos - prev
    next(ps)
    offset = UInt32(ps.t.startbyte - prev)
    return EXPR(fw, offset, UInt32(ps.t.endbyte - ps.t.startbyte + 1), val)
end
