using VerilogAParser
using AbstractTrees
using AbstractTrees: parent, nodevalue
using VerilogAParser.VerilogACSTParser:
    ContributionStatement, AnalogSeqBlock, AnalogBlock,
    InOutDeclaration, NetDeclaration, ParameterDeclaration, AliasParameterDeclaration,
    VerilogModule, Literal, BinaryExpression, BPFC,
    IdentifierPrimary, @case, BranchDeclaration,
    AnalogFunctionDeclaration,
    IntRealDeclaration, AnalogStatement,
    AnalogConditionalBlock, AnalogVariableAssignment, AnalogProceduralAssignment,
    Parens, AnalogIf, AnalogFor, AnalogWhile, AnalogRepeat, UnaryOp, Function,
    AnalogSystemTaskEnable, StringLiteral,
    CaseStatement, FunctionCall, TernaryExpr,
    FloatLiteral, ChunkTree, virtrange,
    filerange, LineNumbers, compute_line,
    SystemIdentifier, Node, Identifier, IdentifierConcatItem,
    IdentifierPart, Attributes
using VerilogAParser.VerilogATokenize:
    Kind, INPUT, OUTPUT, INOUT, REAL, INTEGER, is_scale_factor
using Combinatorics
using ForwardDiff
using ForwardDiff: Dual
using DAECompiler

const VAT = VerilogAParser.VerilogATokenize

const VANode = VerilogAParser.VerilogACSTParser.Node
struct SimTag; end
ForwardDiff.:(≺)(::Type{<:ForwardDiff.Tag}, ::Type{SimTag}) = true
ForwardDiff.:(≺)(::Type{SimTag}, ::Type{<:ForwardDiff.Tag}) = false

function DAECompiler.ddt(dual::ForwardDiff.Dual{SimTag})
    ForwardDiff.Dual{SimTag}(ddt(dual.value), map(ddt, dual.partials.values))
end

function eisa(e::VANode{S}, T::Type) where {S}
    S <: T
end
formof(e::VANode{S}) where {S} = S

@enum BranchKind CURRENT VOLTAGE

struct VAFunction
    arg_order::Vector{Symbol}
    inout_decls::Dict{Symbol, Symbol}
end

struct Scope
    parameters::Set{Symbol}
    node_order::Vector{Symbol}
    ninternal_nodes::Int
    branch_order::Vector{Pair{Symbol}}
    used_branches::Set{Pair{Symbol}}
    var_types::Dict{Symbol, Union{Type{Int}, Type{Float64}}}
    all_functions::Dict{Symbol, VAFunction}
    undefault_ids::Bool
    ddx_order::Vector{Symbol}
end
Scope() = Scope(Set{Symbol}(), Vector{Symbol}(), 0, Vector{Pair{Symbol}}(), Set{Pair{Symbol}}(),
    Dict{Symbol, Union{Type{Int}, Type{Float64}}}(), Dict{Symbol, VAFunction}(),
    false, Vector{Symbol}())

struct Pins
    p::Any
    n::Any
end
Base.reverse(p::Pins) = Pins(p.n, p.p)
Base.getindex(p::Pins, i::Integer) = i == 1 ? p.p : i == 2 ? p.n : throw(BoundsError(p, i))

struct BranchContribution
    kind::BranchKind
    pins::Pins
    value::Any
end

function Base.LineNumberNode(n::VANode)
    print_vr = (n.startof+n.expr.off):(n.startof+n.expr.off+n.expr.width-1)
    tc = AbstractTrees.TreeCursor(ChunkTree(n.ps))
    leaf = VerilogAParser.VerilogACSTParser.findleafbyvirtrange(Leaves(tc), first(print_vr))

    # Compute line number
    vr = virtrange(leaf)
    fr = filerange(leaf)

    startoff_file = first(fr) + (n.startof + n.expr.off - first(vr))

    sf = n.ps.srcfiles[leaf.fidx]
    lsf = sf.lineinfo

    lno_first = compute_line(lsf, startoff_file)
    LineNumberNode(lno_first, Symbol(sf.path))
end


function (::Scope)(cs::VANode{Literal})
    Meta.parse(String(cs))
end

const sf_mapping = Dict(
    'T' => 1e12,
    'G' => 1e9,
    'M' => 1e6,
    'K' => 1e3,
    'k' => 1e3,
    'm' => 1e-3,
    'u' => 1e-6,
    'n' => 1e-9,
    'p' => 1e-12,
    'f' => 1e-15,
    'a' => 1e-18,
);

function (::Scope)(cs::VANode{FloatLiteral})
    txt = String(cs)
    sf = nothing
    if is_scale_factor(txt[end])
        sf = txt[end]
        txt = txt[1:end-1]
    end
    ret = Base.parse(Float64, txt)
    if sf !== nothing
        ret *= sf_mapping[sf]
    end
    return ret
end

function (to_julia::Scope)(cs::VANode{ContributionStatement})
    bpfc = cs.lvalue
    kind = Symbol(bpfc.id)
    # TODO: Look at disciplines
    if kind == :I
        kind = CURRENT
    elseif kind == :V
        kind = VOLTAGE
    else
        return :(error("Unknown branch contribution kind"))
    end

    refs = map(bpfc.references) do ref
        Symbol(assemble_id_string(ref.item))
    end

    if length(refs) == 1
        node = refs[1]
        svar = Symbol("branch_state_", node, "_0")
        eqvar = Symbol("branch_value_", node, "_0")
        push!(to_julia.used_branches, node => Symbol("0"))
        return quote
            if $svar != $kind
                $eqvar = 0.0
                $svar = $kind
            end
            $eqvar += $(ForwardDiff.value)($(SimTag),$(to_julia(cs.assign_expr)))
        end
    elseif length(refs) == 2
        (id1, id2) = refs

        idx = findfirst(to_julia.branch_order) do branch
            branch == (id1 => id2) || branch == (id2 => id1)
        end
        @assert idx !== nothing
        branch = to_julia.branch_order[idx]
        push!(to_julia.used_branches, branch)
        reversed = branch == (id2 => id1)

        s = gensym()

        svar = Symbol("branch_state_", branch[1], "_", branch[2])
        eqvar = Symbol("branch_value_", branch[1], "_", branch[2])
        return @nolines quote
            if $svar != $kind
                $eqvar = 0.0
                $svar = $kind
            end
            $s = $(to_julia(cs.assign_expr))
            $eqvar += $(ForwardDiff.value)($(SimTag), $(reversed ? :(-$s) : s))
        end
    end
end

function (to_julia::Scope)(bpfc::VANode{BPFC})
    Expr(:call, Symbol(bpfc.id), map(x->Symbol(x.item), bpfc.references)...)
end

function assemble_id_string(id)
    if isa(id, Node{SystemIdentifier})
        return String(id)
    elseif isa(id, Node{Identifier})
        return join(assemble_id_string(c) for c in children(id))
    elseif isa(id, Node{IdentifierPart})
        s = String(id)
        id.escaped && (s = 2[2:end])
        return s
    elseif isa(id, Node{IdentifierConcatItem})
        return assemble_id_string(id.id)
    else
        error(typeof(id))
    end
end

function (scope::Scope)(ip::VANode{IdentifierPrimary})
    id = Symbol(assemble_id_string(ip.id))
    if scope.undefault_ids
        id = Expr(:call, undefault, id)
    end
    id
end

function (scope::Scope)(ip::VANode{SystemIdentifier})
    id = Symbol(ip)
    # Is this really the right place for these? These are kind function calls
    # without arguments in the spec.
    if id == Symbol("\$temperature")
        return Expr(:call, id)
    else
        error()
    end
end

function (to_julia::Scope)(cs::VANode{BinaryExpression})
    op = Symbol(cs.op)
    #if op in (:(||), :(&&))
    #    return Expr(op, to_julia(cs.lhs), to_julia(cs.rhs))
    if op == :(||)
        return Expr(:call, (|), to_julia(cs.lhs), to_julia(cs.rhs))
    elseif op == :(&&)
        return Expr(:call, (&), to_julia(cs.lhs), to_julia(cs.rhs))
    else
        return Expr(:call, op, to_julia(cs.lhs), to_julia(cs.rhs))
    end
end

function (to_julia::Scope)(asb::VANode{AnalogSeqBlock})
    if asb.decl !== nothing
        block_var_types = copy(to_julia.var_types)

        for decl in asb.decl.decls
            item = decl.item
            @case formof(item) begin
                IntRealDeclaration => begin
                    T = kw_to_T(item.kw.kw)
                    for name in item.idents
                        block_var_types[Symbol(name.item)] = T
                    end
                end
            end
        end

        to_julia_block = Scope(to_julia.parameters,
            to_julia.node_order, to_julia.ninternal_nodes, to_julia.branch_order,
            to_julia.used_branches, block_var_types, to_julia.all_functions,
            to_julia.undefault_ids, to_julia.ddx_order)
    else
        to_julia_block = to_julia
    end
    ret = Expr(:block)
    last_lno = nothing
    for stmt in asb.stmts
        lno = LineNumberNode(stmt)
        if lno != last_lno
            push!(ret.args, lno)
        end
        push!(ret.args, to_julia_block(stmt))
    end
    ret
end

abstract type SemaError <: CedarException; end

struct DuplicateDef <: SemaError
    name::Symbol
end

struct NoArguments <: SemaError; end
struct MissingTypeDecl <: SemaError
    name::Symbol
end


function validate_parameters(type_decls, inout_decls)
    isempty(inout_decls) && throw(NoArguments())
    for key in keys(inout_decls)
        haskey(type_decls, key) || throw(MissingTypeDecl(key))
    end
end

kw_to_T(kw::Kind) = kw === REAL ? Float64 : Int

(to_julia::Scope)(stmt::VANode{AnalogStatement}) = to_julia(stmt.stmt)

function (to_julia::Scope)(stmt::VANode{AnalogConditionalBlock})
    aif = stmt.aif
    function if_body_to_julia(ifstmt)
        if formof(ifstmt) == AnalogSeqBlock
            return to_julia(ifstmt)
        else
            return Expr(:block, LineNumberNode(ifstmt), to_julia(ifstmt))
        end
    end

    ifex = ex = Expr(:if, to_julia(aif.condition), if_body_to_julia(aif.stmt))
    for case in stmt.elsecases
        if formof(case.stmt) == AnalogIf
            elif = case.stmt
            newex = Expr(:elseif, to_julia(elif.condition), if_body_to_julia(elif.stmt))
            push!(ex.args, newex)
            ex = newex
        else
            push!(ex.args, if_body_to_julia(case.stmt))
        end
    end
    ifex
end

function (to_julia::Scope)(stmt::VANode{AnalogFor})
    body = to_julia(stmt.stmt)
    push!(body.args, to_julia(stmt.update_stmt))
    while_expr = Expr(:while, to_julia(stmt.cond_expr), body)
    Expr(:block, to_julia(stmt.init_stmt), while_expr)
end

function (to_julia::Scope)(stmt::VANode{AnalogWhile})
    body = to_julia(stmt.stmt)
    Expr(:while, to_julia(stmt.cond_expr), body)
end

function (to_julia::Scope)(stmt::VANode{AnalogRepeat})
    body = to_julia(stmt.stmt)
    Expr(:for, :(_ = 1:$(stmt.num_repeat)), body)
end

function (to_julia::Scope)(stmt::VANode{UnaryOp})
    return Expr(:call, Symbol(stmt.op), to_julia(stmt.operand))
end

function (to_julia::Scope)(stmt::VANode{FunctionCall})
    fname = Symbol(stmt.id)
    if fname == Symbol("\$param_given")
        id = Symbol(stmt.args[1].item)
        return Expr(:call, !, Expr(:call, isdefault,
            Expr(:call, getfield, Symbol("#self#"), QuoteNode(id))))
    end

    # TODO: Rather than hardcoding this, this should look at the list of
    # defined disciplines
    if fname == :V
        function Vref(id)
            id_idx = findfirst(==(id), to_julia.ddx_order)
            if id_idx !== nothing
                return Expr(:call, Dual{SimTag}, id,
                        Expr(:(...), ntuple(i->i == id_idx ? 1.0 : 0.0, length(to_julia.ddx_order)))
                )
            else
                return id
            end
        end

        @assert length(stmt.args) in (1,2)
        id1 = Symbol(stmt.args[1].item)
        id2 = length(stmt.args) > 1 ? Symbol(stmt.args[2].item) : nothing

        if id2 === nothing
            push!(to_julia.used_branches, id1 => Symbol("0"))
            return Vref(id1)
        else
            idx = findfirst(to_julia.branch_order) do branch
                branch == (id1 => id2) || branch == (id2 => id1)
            end
            @assert idx !== nothing
            branch = to_julia.branch_order[idx]
            push!(to_julia.used_branches, branch)
            return :($(Vref(id1)) - $(Vref(id2)))
        end
    elseif fname == :I
        @assert length(stmt.args) == 2
        id1 = Symbol(stmt.args[1].item)
        id2 = Symbol(stmt.args[2].item)

        idx = findfirst(to_julia.branch_order) do branch
            branch == (id1 => id2) || branch == (id2 => id1)
        end
        @assert idx !== nothing

        branch = to_julia.branch_order[idx]
        reversed = branch == (id2 => id1)
        push!(to_julia.used_branches, branch)

        ex = :(error("TODO"))
        reversed && (ex = :(-$ex))
        return ex
    elseif fname == :ddx
        item = stmt.args[2].item
        @assert formof(item) == FunctionCall
        @assert Symbol(item.id) == :V
        if length(item.args) == 1
            probe = Symbol(item.args[1].item)
            id_idx = findfirst(==(probe), to_julia.ddx_order)
            return :(let x = $(to_julia(stmt.args[1].item))
                $(isa)(x, $(Dual)) ? (@inbounds $(ForwardDiff.partials)($(SimTag), x, $id_idx)) : 0.0
            end)
        else
            probe1 = Symbol(item.args[1].item)
            id1_idx = findfirst(==(probe1), to_julia.ddx_order)
            probe2 = Symbol(item.args[2].item)
            id2_idx = findfirst(==(probe2), to_julia.ddx_order)
            return :(let x = $(to_julia(stmt.args[1].item)),
                        dx1 = $(isa)(x, $(Dual)) ? (@inbounds $(ForwardDiff.partials)($(SimTag), x, $id1_idx)) : 0.0,
                        dx2 = $(isa)(x, $(Dual)) ? (@inbounds $(ForwardDiff.partials)($(SimTag), x, $id2_idx)) : 0.0
                (dx1-dx2)/2
            end)
        end
    elseif fname ∈ (:white_noise, :flicker_noise)
        args = map(x->to_julia(x.item), stmt.args)
        ϵ = Symbol(:ϵ, gensym(last(args)))
        args[end] = QuoteNode(ϵ)
        return Expr(:call, fname, :dscope, args...)
    end

    vaf = get(to_julia.all_functions, fname, nothing)
    if vaf !== nothing
        # Call to a Verilog-A defined function
        args = map(x->to_julia(x.item), stmt.args)
        in_args = Any[]
        out_args = Any[]

        if length(args) != length(vaf.arg_order)
            # TODO: Fancy diagnostic
            return Expr(:call, error, "Wrong number of arguments to function $fname ($args, $(vaf.arg_order)")
        end

        for (arg, vaarg) in zip(args, vaf.arg_order)
            kind = vaf.inout_decls[vaarg]
            if kind == :output || kind == :inout
                isa(arg, Symbol) || return Expr(:call, error, "Output argument $vaarg to function $fname must be a symbol. Got $arg.")
                push!(out_args, arg)
            end
            if kind == :input || kind == :inout
                push!(in_args, arg)
            end
        end
        ret = Expr(:call, fname, in_args...)
        if length(out_args) != 0
            s = gensym()
            ret = @nolines quote
                ($s, ($(out_args...),)) = $ret
                $s
            end
        end
        return ret
    end

    return Expr(:call, fname, map(x->to_julia(x.item), stmt.args)...)
end

function (to_julia::Scope)(stmt::VANode{AnalogProceduralAssignment})
    return to_julia(stmt.assign)
end
function (to_julia::Scope)(stmt::VANode{AnalogVariableAssignment})
    assignee = Symbol(stmt.lvalue)
    varT = get(to_julia.var_types, assignee, nothing)
    assignee === nothing && cedarerror("Undeclared variable: $assignee")

    eq = stmt.eq.op

    op = eq == VAT.EQ             ?  :(=) :
         eq == VAT.STAR_STAR_EQ   ?  :(=) : # Extra handling below
         eq == VAT.PLUS_EQ        ? :(+=) :
         eq == VAT.MINUS_EQ       ? :(-=) :
         eq == VAT.STAR_EQ        ? :(*=) :
         eq == VAT.SLASH_EQ       ? :(/=) :
         eq == VAT.PERCENT_EQ     ? :(%=) :
         eq == VAT.AND_EQ         ? :(&=) :
         eq == VAT.OR_EQ          ? :(|=) :
         eq == VAT.XOR_EQ         ? :(^=) :
         eq == VAT.LBITSHIFT_EQ   ? :(<<=) :
         eq == VAT.RBITSHIFT_EQ   ? :(>>=) :
         eq == VAT.LBITSHIFT_A_EQ ? :(=) : # Extra handling below
         eq == VAT.RBITSHIFT_A_EQ ? :(>>>=) :
         cedarerror("Unsupported assignment operator: $eq")

    req = Expr(op, assignee, varT === nothing ? Expr(:call, error, "Unknown type for variable $assignee") :
        Expr(:call, VerilogAEnvironment.vaconvert, varT, to_julia(stmt.rvalue)))

    if eq == VAT.STAR_STAR_EQ
        req = Expr(op, assignee, Expr(:call, var"**", req.args...))
    elseif eq === VAT.LBITSHIFT_A_EQ
        req = Expr(op, assignee, Expr(:call, var"<<<", req.args...))
    end

    return req
end

function (to_julia::Scope)(stmt::VANode{Parens})
    return to_julia(stmt.inner)
end

function (to_julia::Scope)(cs::VANode{TernaryExpr})
    return Expr(:if, to_julia(cs.condition), to_julia(cs.ifcase), to_julia(cs.elsecase))
end


function (to_julia::Scope)(fd::VANode{AnalogFunctionDeclaration})
    type_decls = Dict{Symbol, Any}()
    inout_decls = Dict{Symbol, Symbol}()
    fname = Symbol(fd.id)
    var_types = Dict{Symbol, Union{Type{Int}, Type{Float64}}}()
    # 4.7.1
    # The analog_function_type specifies the return value of the function;
    # its use is optional. type can be a real or an integer; if unspecified,
    # the default is real.
    rt = fd.fty === nothing ? Real : kw_to_T(fd.fty.kw)
    var_types[fname] = rt
    arg_order = Symbol[]
    for decl in fd.items
        item = decl.item
        @case formof(item) begin
            InOutDeclaration => begin
                kind = item.kw.kw === INPUT ?  :input :
                       item.kw.kw === OUTPUT ? :output :
                                               :inout
                for name in item.portnames
                    ns = Symbol(name.item)
                    haskey(inout_decls, ns) && throw(DuplicateDef(name))
                    # NB: Don't think this is technically forbidden by the standard
                    ns == fname && throw(DuplicateDef(ns))
                    inout_decls[ns] = kind
                    push!(arg_order, ns)
                end
            end
            IntRealDeclaration => begin
                T = kw_to_T(item.kw.kw)
                for name in item.idents
                    ns = Symbol(name.item)
                    haskey(type_decls, ns) && throw(DuplicateDef(name))
                    ns == fname && throw(DuplicateDef(ns))
                    var_types[Symbol(name.item)] = T
                end
            end
            _ => cedarerror("Unknown function declaration item")
        end
    end

    to_julia_internal = Scope(to_julia.parameters, to_julia.node_order,
        to_julia.ninternal_nodes, to_julia.branch_order, to_julia.used_branches, var_types,
        to_julia.all_functions, to_julia.undefault_ids, to_julia.ddx_order)

    validate_parameters(var_types, inout_decls)
    out_args = [k for k in arg_order if inout_decls[k] in (:output, :inout)]
    in_args = [k for k in arg_order if inout_decls[k] in (:input, :inout)]
    rt_decl = length(out_args) == 0 ? fname : :(($fname, ($(out_args...),)))

    to_julia.all_functions[fname] = VAFunction(arg_order, inout_decls)

    localize_vars = Any[]
    for var in keys(var_types)
        var in arg_order && continue
        push!(localize_vars, :(local $var))
    end

    return @nolines quote
        @inline function $fname($(in_args...))
            $(localize_vars...)
            local $fname = $(VerilogAEnvironment.vaconvert)($rt, 0)
            $(to_julia_internal(fd.stmt))
            return $rt_decl
        end
    end
end

function (to_julia::Scope)(stmt::VANode{StringLiteral})
    return String(stmt)[2:end-1]
end

const systemtaskenablemap = Dict{Symbol, Function}(
    Symbol("\$ln")=>Base.log, Symbol("\$log10")=>log10, Symbol("\$exp")=>exp, Symbol("\$sqrt")=>sqrt,
    Symbol("\$sin")=>sin, Symbol("\$cos")=>cos, Symbol("\$tan")=>tan,
    Symbol("\$asin")=>asin, Symbol("\$acos")=>acos, Symbol("\$atan")=>atan, Symbol("\$atan2")=>atan,
    Symbol("\$sinh")=>sinh, Symbol("\$cos")=>cosh, Symbol("\$tan")=>tanh,
    Symbol("\$asinh")=>asinh, Symbol("\$acosh")=>acosh, Symbol("\$atanh")=>atanh,
    Symbol("\$error")=>error)
function (to_julia::Scope)(stmt::VANode{AnalogSystemTaskEnable})
    if formof(stmt.task) == FunctionCall
        fc = stmt.task
        fname = Symbol(fc.id)
        args = map(x->to_julia(x.item), fc.args)
        if fname in keys(systemtaskenablemap)
            return Expr(:call, systemtaskenablemap[fname], args...)
        elseif fname == Symbol("\$strobe")
            # TODO: Need to guard this with convergence conditions
            return nothing # Expr(:call, println, args...)
        elseif fname == Symbol("\$warning")
            warn = GlobalRef(Base, Symbol("@warn"))
            return nothing # Expr(:macrocall, warn, LineNumberNode(stmt), args..., :(maxlog=1))
        else
            cedarerror("Verilog task unimplmented: $fname")
        end
    else
        error()
    end
end

function (to_julia::Scope)(stmt::VANode{CaseStatement})
    s = gensym()
    first = true
    expr = nothing
    default_case = nothing
    for case in stmt.cases
        if isa(case.conds, Node)
            # Default case
            default_case = to_julia(case.item)
        else
            conds = map(cond->:($s == $(to_julia(cond.item))), case.conds)
            cond = length(conds) == 1 ? conds[1] : Expr(:(||), conds...)
            ex = Expr(first ? :if : :elseif, cond, to_julia(case.item))
            if first
                expr = ex
                first = false
            else
                push!(expr.args, ex)
            end
        end
    end
    default_case !== nothing && (push!(expr.args, default_case))
    Expr(:block, :($s = $(to_julia(stmt.switch))), expr)
end

function (to_julia::Scope)(stmt::VANode{Attributes})
    res = Dict{Symbol, Any}()
    for attr in stmt.specs
        item = attr.item
        res[Symbol(item.name)] = to_julia(item.val)
    end
    return res
end

function pins(vm::VANode{VerilogModule})
    plist = vm.port_list
    plist === nothing && return []
    pins = Symbol[]
    mapreduce(vcat, plist.ports) do port_decl
        Symbol(port_decl.item)
    end
end

using Base.Meta
using Core.Compiler: SlotNumber

function find_ddx!(ddx_order::Vector{Symbol}, va::VANode)
    for stmt in AbstractTrees.PreOrderDFS(va)
        if stmt isa VANode{FunctionCall} && Symbol(stmt.id) == :ddx
            item = stmt.args[2].item
            @assert formof(item) == FunctionCall
            @assert Symbol(item.id) == :V
            for arg in item.args
                name = Symbol(arg.item)
                !in(name, ddx_order) && push!(ddx_order, Symbol(arg.item))
            end
        end
    end
end

function make_spice_device(vm::VANode{VerilogModule})
    ps = pins(vm)
    modname = String(vm.id)
    symname = Symbol(modname)
    ret = Expr(:block,
        #map(ps) do p
        #    :(@named $p = Pin(analysis))
        #end...
    )
    struct_fields = Any[]
    defaults = Any[]
    parameter_names = Set{Symbol}()
    to_julia_global = Scope()
    find_ddx!(to_julia_global.ddx_order, vm)
    to_julia_defaults = Scope(to_julia_global.parameters,
    to_julia_global.node_order, to_julia_global.ninternal_nodes,
        to_julia_global.branch_order, to_julia_global.used_branches,
        to_julia_global.var_types, to_julia_global.all_functions,
        true, to_julia_global.ddx_order)
    internal_nodes = Vector{Symbol}()
    var_types = Dict{Symbol, Union{Type{Int}, Type{Float64}}}()
    aliases = Dict{Symbol, Symbol}()
    observables = Dict{Symbol, Symbol}()

    # First to a pre-pass to figure out the scope context
    for child in vm.items
        item = child.item
        @case formof(item) begin
            # Not represented on the julia side for now
            InOutDeclaration => nothing
            NetDeclaration => begin
                for net in item.net_names
                    id = Symbol(assemble_id_string(net.item))
                    if !(id in ps)
                        # Internal node
                        push!(internal_nodes, id)
                    end
                end
            end
            ParameterDeclaration => begin
                for param in item.params
                    param = param.item
                    pT = Float64
                    if item.ptype !== nothing
                        pT = kw_to_T(item.ptype.kw)
                    end
                    paramname = String(assemble_id_string(param.id))
                    paramname_lc = lowercase(paramname)
                    paramsym = Symbol(paramname)
                    push!(parameter_names, paramsym)
                    # TODO: The CMC Verilog-A models use an attribute to
                    # distinguish between model and instance parameters
                    push!(struct_fields,
                        :($(Symbol(paramsym))::$(DefaultOr{pT}) = $(Expr(:block,
                            LineNumberNode(param.default_expr),
                            to_julia_defaults(param.default_expr)))))
                    var_types[Symbol(paramname)] = pT
                    #push!(ret.args,
                    #    :(@parameters $(Symbol(paramsym)))
                    #)
                    #push!(defaults,
                    #    :($(Symbol(paramsym)) => instance.model.$(paramsym))
                    #)
                    #push!(parameter_names, paramsym)
                    # TODO: SPICE likes these lowercase, so we generate both,
                    # but in Verilog-A they're case sensitive, so shouldn't
                    # interact.
                    #if paramname != paramname_lc
                    #    push!(ret.args, :($(Symbol(paramname)) = $(Symbol(paramname_lc))))
                    #end
                end
            end
            AliasParameterDeclaration => begin
                param = item
                paramsym = Symbol(assemble_id_string(param.id))
                targetsym = Symbol(assemble_id_string(param.value))
                push!(parameter_names, paramsym)
                aliases[paramsym] = targetsym
            end
            IntRealDeclaration => begin
                attr = child.attrs !== nothing && to_julia_global(child.attrs)
                observe = attr isa Dict && length(attr) == 1 && haskey(attr, :desc)
                T = kw_to_T(item.kw.kw)
                for ident in item.idents
                    name = Symbol(String(ident.item))
                    var_types[name] = T
                    if observe
                        observables[name] = Symbol(attr[:desc])
                    end
                end
            end
        end
    end

    node_order = [ps; internal_nodes; Symbol("0")]
    to_julia = Scope(parameter_names,  node_order, length(internal_nodes),
        collect(map(x->Pair(x...), combinations(node_order, 2))),
        Set{Pair{Symbol}}(),
        var_types,
        Dict{Symbol, VAFunction}(), false,
        to_julia_global.ddx_order)
    lno = nothing
    for child in vm.items
        item = child.item
        @case formof(item) begin
            # Not represented on the julia side for now
            InOutDeclaration => nothing
            IntRealDeclaration => nothing
            NetDeclaration => nothing # Handled above
            BranchDeclaration => nothing
            ParameterDeclaration => nothing # Handled above
            AliasParameterDeclaration => nothing # Handled above
            AnalogFunctionDeclaration => begin
                push!(ret.args, to_julia(item))
            end
            AnalogBlock => begin
                lno = LineNumberNode(item.stmt)
                push!(ret.args, to_julia(item.stmt))
            end
            _ => cedarerror("Unrecognized statement $child")
        end
    end

    internal_nodeset = map(enumerate(internal_nodes)) do (n, id)
        @nolines quote
            $id = $(DAECompiler.variable)($DScope(dscope, $(QuoteNode(Symbol("V($id)")))))
        end
    end

    all_branch_order = filter(branch->branch in to_julia.used_branches, to_julia.branch_order)
    branch_state = map(all_branch_order) do (a, b)
        svar = Symbol("branch_state_", a, "_", b)
        eqvar = Symbol("branch_value_", a, "_", b)
        @nolines quote
            $svar = $(CURRENT)
            $eqvar = 0.0
        end
    end

    internal_currents = Any[(Symbol("I($a, $b)") for (a, b) in all_branch_order)...]

    internal_currents_def = Expr(:block,
        (@nolines quote
            $v = $(DAECompiler.variable)($DScope(dscope,$(QuoteNode(v))))
        end for v in internal_currents)...
    )

    function current_sum(node)
        Expr(:call, +, map(Iterators.filter(branch->node in branch[2], enumerate(all_branch_order))) do (n, (a,b))
            ex = internal_currents[n]
            # Positive currents flow out of devices, into nodes, so I(a, b)'s contribution to the a KCL
            # is -I(a, b).
            node == a ? :(-$ex) : ex
        end...)
    end

    internal_node_kcls = map(enumerate(internal_nodes)) do (n, node)
        @nolines :($(DAECompiler.equation!)($(current_sum(node)),
            $DScope(dscope, $(QuoteNode(Symbol("KCL($node)"))))))
    end

    internal_eqs = map(enumerate(all_branch_order)) do (n, (a,b))
        svar = Symbol("branch_state_", a, "_", b)
        eqvar = Symbol("branch_value_", a, "_", b)
        if b == Symbol("0")
            @nolines :($(DAECompiler.equation!)($svar == $(CURRENT) ? $(internal_currents[n]) - $eqvar : $a - $eqvar,
                $DScope(dscope, $(QuoteNode(Symbol("Branch($a)"))))))
        else
            @nolines :($(DAECompiler.equation!)($svar == $(CURRENT) ? $(internal_currents[n]) - $eqvar : ($a - $b) - $eqvar,
                $DScope(dscope, $(QuoteNode(Symbol("Branch($a, $b)"))))))
        end
    end

    argnames = map(p->Symbol("#port_", p), ps)
    external_eqs = map(argnames, map(current_sum, ps)) do a, c
        :($(kcl!)($(a), $c))
    end

    obs_def = (:($o = 0.0) for o in keys(observables))
    obs_expr = (:($(DAECompiler.observed!)($var,
            $DScope(dscope, $(QuoteNode(name))))) for (var, name) in observables)

    arg_assign = map(ps, argnames) do p, a
        :($p = $a.V)
    end

    params_to_locals = map(collect(to_julia.parameters)) do id
        :($id = $(Expr(:call, undefault, Expr(:call, getfield, Symbol("#self#"), QuoteNode(id)))))
    end

    sim = @nolines :(function (var"#self#"::$symname)($(argnames...); dscope=$(GenScope)($(debug_scope)[], $(QuoteNode(symname))))
        $(obs_def...)
        $lno
        $(arg_assign...)
        $(internal_currents_def)
        $(params_to_locals...)
        $(internal_nodeset...)
        $(branch_state...)
        $ret
        $(internal_node_kcls...)
        $(external_eqs...)
        $(internal_eqs...)
        $(obs_expr...)
        return ()
    end)

    Expr(:toplevel,
        :(VerilogAEnvironment.CedarSim.@kwdef struct $symname <: VerilogAEnvironment.VAModel
            $(struct_fields...)
        end),
        sim,
    )
end


struct VAFile
    file::String
end
Base.String(vaf::VAFile) = vaf.file
Base.abspath(file::VAFile) = VAFile(Base.abspath(file.file))
Base.isfile(file::VAFile) = Base.isfile(file.file)
Base.isabspath(file::VAFile) = Base.isabspath(file.file)
Base.findfirst(str::String, file::VAFile) = Base.findfirst(str, file.file)
Base.joinpath(str::String, file::VAFile) = VAFile(Base.joinpath(str, file.file))
Base.normpath(file::VAFile) = VAFile(Base.normpath(file.file))
export VAFile, @va_str

function make_module(va::VANode)
    vamod = va.stmts[end]
    s = Symbol(String(vamod.id), "_module")
    Expr(:toplevel, :(baremodule $s
        using ..CedarSim.VerilogAEnvironment
        export $(Symbol(vamod.id))
        $(CedarSim.make_spice_device(vamod))
    end), :(using .$s))
end

function parse_and_eval_vafile(mod::Module, file::VAFile)
    va = VerilogAParser.parsefile(file.file)
    if va.ps.errored
        cedarthrow(LoadError(file.file, 0, VAParseError(va)))
    else
        Core.eval(mod, make_module(va))
    end
    return va.ps.srcfiles
end

function Base.include(mod::Module, file::VAFile)
    parse_and_eval_vafile(mod, file)
    return nothing
end

macro va_str(str)
    va = VerilogAParser.parse(IOBuffer(str))
    if va.ps.errored
        cedarthrow(LoadError("va_str", 0, VAParseError(va)))
    else
        esc(make_module(va))
    end
end

struct VAParseError
    va
end

Base.show(io::IO, vap::VAParseError) = VerilogAParser.VerilogACSTParser.visit_errors(vap.va; io)
