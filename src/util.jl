using DAECompiler.Intrinsics: ddt

abstract type CedarException <: Exception end
# Always supress stack-trace on CedarExceptions as they will shouw details of out internals that are
# confounding to end users. (Instead such exceptions must be self contained enough to be interpretted without stacktrack)
Base.showerror(io::IO, ex::CedarException, bt; backtrace=true) = showerror(io, ex)

struct WrappedCedarException{T<:Exception} <: CedarException
    err::T
end

Base.showerror(io::IO, err::WrappedCedarException) = showerror(io, err.err)

struct CedarError <: CedarException
    msg::String
end

Base.showerror(io::IO, err::CedarError) = println(io, err.msg)

cedarthrow(err) = throw(WrappedCedarException(err))
cedarerror(msg) = throw(CedarError(msg))

struct Default{T}
    val::T
end
Default(d::Default) = d
undefault(val::Default) = val.val
undefault(val) = val
isdefault(val::Default) = true

# Base's Unions are unfortuantely, quite slow to dispatch on,
# so we have this struct to explicitly emulate Union{Default{T}, T}.
struct DefaultOr{T}
    val::T
    is_default::Bool
    DefaultOr{T}(val::T, is_default::Bool) where {T} = new{T}(val, is_default)
    # Hack to make sure the compiler can see the purity of this call
    global mkdefault
    @eval DefaultOr(val::T, is_default::Bool) where {T} = $(Expr(:new, :(DefaultOr{T}), :val, :is_default))
    @eval mkdefault(t::T) where {T} = $(Expr(:new, :(DefaultOr{T}), :t, true))
end
Base.convert(::Type{DefaultOr{T}}, x::DefaultOr{T}) where {T} = x
Base.convert(::Type{DefaultOr{S}}, x::DefaultOr{T}) where {S, T} = DefaultOr{S}(convert(S, x.val), x.is_default)
Base.convert(::Type{DefaultOr{T}}, x::T) where {T} = DefaultOr{T}(x, false)
Base.convert(::Type{DefaultOr{T}}, x) where {T} = DefaultOr{T}(convert(T, x), false)
Base.convert(::Type{T}, x::DefaultOr) where {T} = Base.convert(T, x.val)
DefaultOr(d::DefaultOr, is_default::Bool) = DefaultOr(d.val, is_default)
DefaultOr(d::DefaultOr) = DefaultOr(d.val, true)
mknondefault(t::T) where {T} = DefaultOr{T}(t, false)
isdefault(x::DefaultOr) = x.is_default
undefault(x::DefaultOr) = x.val

macro fieldaliases(type, arg)
    esc(quote
    let mapping = Dict($arg)
        function Base.getproperty(o::$type, s::Symbol)
            if haskey(mapping, s)
                s = mapping[s]
            end
            getfield(o, s)
        end
        function Base.setproperty!(o::$type, s::Symbol, v)
            if haskey(mapping, s)
                s = mapping[s]
            end
            setfield!(o, s, v)
        end
        function Base.propertynames(o::$type)
            [fieldnames($type)...
            keys(mapping)...]
        end
    end
    end)
end

# like Base.remove_linenums!, but only removes line numbers that are from the same file
function remove_local_linenums!(@nospecialize(ex), lnn::LineNumberNode)
    if ex isa Expr
        if ex.head === :block || ex.head === :quote
            # remove line number expressions from metadata (not argument literal or inert) position
            filter!(ex.args) do x
                isa(x, Expr) && x.head === :line && return false
                isa(x, LineNumberNode) && x.file == lnn.file && return false
                return true
            end
        end
        for subex in ex.args
            subex isa Expr && remove_local_linenums!(subex, lnn)
        end
        return ex
    elseif ex isa CodeInfo
        ex.codelocs .= 0
        length(ex.linetable) > 1 && resize!(ex.linetable, 1)
        return ex
    else
        return ex
    end
end

macro nolines(ex)
    esc(remove_local_linenums!(ex, __source__))
end

macro kwdef(expr)
    expr = macroexpand(__module__, expr) # to expand @static
    expr isa Expr && expr.head === :struct || error("Invalid usage of @kwdef")
    expr = expr::Expr
    T = expr.args[2]
    if T isa Expr && T.head === :<:
        T = T.args[1]
    end

    def_params_ex = Expr(:parameters)
    params_ex = Expr(:parameters)
    call_args = Any[]
    code = Any[]

    _kwdef!(expr.args[3], T, def_params_ex.args, params_ex.args, call_args, code)
    # Only define a constructor if the type has fields, otherwise we'll get a stack
    # overflow on construction
    if !isempty(params_ex.args)
        if T isa Symbol
            kwdefs = @nolines quote
                function ($(esc(T)))($params_ex)
                    $(code...)
                    ($(esc(T)))($(call_args...))
                end
                ($(esc(T)))($def_params_ex, cur::($(esc(T)))) = ($(esc(T)))($(call_args...))
            end
        elseif T isa Expr && T.head === :curly
            error("TODO")
            T = T::Expr
            # if T == S{A<:AA,B<:BB}, define two methods
            #   S(...) = ...
            #   S{A,B}(...) where {A<:AA,B<:BB} = ...
            S = T.args[1]
            P = T.args[2:end]
            Q = Any[U isa Expr && U.head === :<: ? U.args[1] : U for U in P]
            SQ = :($S{$(Q...)})
            kwdefs = quote
                ($(esc(S)))($params_ex) =($(esc(S)))($(call_args...))
                ($(esc(SQ)))($params_ex) where {$(esc.(P)...)} =
                    ($(esc(SQ)))($(call_args...))
            end
        else
            error("Invalid usage of @kwdef")
        end
    else
        kwdefs = nothing
    end
    @nolines quote
        Base.@__doc__($(esc(expr)))
        $kwdefs
    end
end

notunion(::Type{Union{S, Default{S}}}) where {S} = S
notdefault(::Type{DefaultOr{T}}) where {T} = T

# @kwdef helper function
# mutates arguments inplace
function _kwdef!(blk, Tname, def_params_args, params_args, call_args, code)
    code_postfix = Any[]
    for i in eachindex(blk.args)
        ei = blk.args[i]
        if ei isa Symbol
            #  var
            push!(params_args, ei)
            push!(call_args, ei)
        elseif ei isa Expr
            if ei.head === :(=)
                lhs = ei.args[1]
                if lhs isa Symbol
                    #  var = defexpr
                    var = lhs
                elseif lhs isa Expr && lhs.head === :(::) && lhs.args[1] isa Symbol
                    #  var::T = defexpr
                    var = lhs.args[1]
                else
                    # something else, e.g. inline inner constructor
                    #   F(...) = ...
                    continue
                end
                defexpr = ei.args[2]  # defexpr
                lno = nothing
                if isexpr(defexpr, :block) && isa(defexpr.args[1], LineNumberNode)
                    lno = popfirst!(defexpr.args)
                end
                push!(params_args, Expr(:kw, var, @nolines quote
                    $lno
                    $(mkdefault)($(esc(defexpr)))
                end))
                push!(def_params_args, Expr(:kw, var, :(cur.$var)))
                vT = :($(notdefault)(fieldtype($(esc(Tname)), $(QuoteNode(var)))))
                push!(call_args, :($(VerilogAEnvironment.vaconvert)($vT, $(esc(var)))))
                blk.args[i] = lhs
            elseif ei.head === :(::) && ei.args[1] isa Symbol
                # var::Typ
                var = ei.args[1]
                push!(params_args, var)
                push!(def_params_args, Expr(:kw, var, :(cur.$var)))
                push!(call_args, var)
            elseif ei.head === :block
                # can arise with use of @static inside type decl
                # or when pepijn is doing dumb shit with SPICE AST
                _kwdef!(ei, Tname, def_params_args, params_args, call_args, code)
            end
        end
    end
    append!(code, code_postfix)
    blk
end



# Helper functions for figuring out if an optic is applicable to a type:
# X-ref: https://github.com/JuliaObjects/Accessors.jl/issues/92
using Accessors: PropertyLens
function Base.hasproperty(x, o::PropertyLens)
    return typeof(o).parameters[1] ∈ propertynames(x)
end
function Base.hasproperty(x, o::ComposedFunction)
    return Base.hasproperty(x, o.inner) &&
           Base.hasproperty(o.inner(x), o.outer)
end

function tlshow(expr)
    Base.remove_linenums!(expr)
    for e in expr.args
        if e isa Expr && e.head == :toplevel
            tlshow(e)
        else
            show(e)
            print("\n")
        end
    end
end

function default_name_map(sys)
    # Collect all top-level nodes that are terminal:
    top_level_nodes = Set(filter(n -> isempty(propertynames(getproperty(sys, n))), propertynames(sys)))
    # Strip `node_` from the front if there's no conflicting other node:
    name_map = Dict{Symbol,String}()
    for name in top_level_nodes
        # Only allow variables and observed
        level = getfield(sys, :result).names[name]
        (level.var !== nothing || level.obs !== nothing) || continue

        str_name = string(name)
        if startswith(str_name, "node_") && str_name[6:end] ∉ top_level_nodes
            str_name = string(str_name[6:end])
        end
        # Filter out anything named `gnd` or `0`
        if str_name ∈ ("0", "gnd", "GND", "node_0", "node_gnd", "node_GND")
            continue
        end
        name_map[name] = str_name
    end
    return name_map
end

function explore end
