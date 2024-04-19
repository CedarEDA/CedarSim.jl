using Accessors
using AxisKeys
using OrdinaryDiffEq, SciMLBase
using Base.Iterators

export alter, dc!, tran!, Sweep, CircuitSweep, ProductSweep, TandemSweep, SerialSweep, sweepvars, split_axes, sweepify

# This `alter()` is to make it easy to apply directly to a struct or named tuple with the optics.
function alter(x::T, params) where {T}
    lens(selector::Union{PropertyLens,ComposedFunction}) = selector
    lens(selector::Symbol) = Accessors.opticcompose(PropertyLens.(Symbol.(split(string(selector), ".")))...)

    if isa(params, NamedTuple)
        params = pairs(params)
    end

    for (selector, value) in params
        # If `value` is `nothing`, ignore it.  This is our sentinel for "use default value".
        if value === nothing
            continue
        end

        l = lens(selector)
        # Auto-convert other numeric types to Float
        # Ideally, we'd do something more clever at sweep creation time,
        # to automatically convert e.g. `1:10` in a sweep to `Float64.(1:10)`
        # but we'd only want to do that if we knew the type of the thing being swept.
        # Without the machinery to go from parameter overrides to types (within a
        # function circuit or a struct) we can't safely do that.  In practice,
        # at the moment of this writing, we're only ever sweeping over floats,
        # so I expect this `if` statement to get triggered essentially always.
        if isa(value, Number) && isa(l(x), Float64)
            value = Float64(value)
        end
        x = Accessors.set(x, l, value)::T
    end
    return x::T
end

# Special flattening iterator that takes combined Sweeps
# and flattens them to a proper tuple: ((a, b), c) -> (a, b, c)
"""
    SweepFlattener

Flattening iterator that takes combined iterators and flattens them to a proper
tuple.  Allows nesting of sweep types while still providing a flat result.
If the iterators would result in `((a, b), c)` instead returns `(a, b, c)`.

This type is automatically used by [`ProductSweep`](@ref), [`TandemSweep`](@ref)
and [`SerialSweep`](@ref), it is not intended for use by end-users.
"""
struct SweepFlattener{T}
    iterator::T
end
Base.length(sf::SweepFlattener) = Base.length(sf.iterator)
Base.size(sf::SweepFlattener) = Base.size(sf.iterator)
Base.size(sf::SweepFlattener, d::Integer) = get(size(sf), d, 1)
Base.IteratorSize(sf::SweepFlattener) = Base.IteratorSize(sf.iterator)
sweepvars(sf::SweepFlattener) = sweepvars(sf.iterator)
sweep_example(sf::SweepFlattener) = isempty(sf) ? () : first(sf)

function Base.show(io::IO, sf::SweepFlattener)
    indent = get(io, :indent, 0)
    indent_str = " "^indent
    if isa(sf.iterator, Base.Iterators.ProductIterator)
        print(io, indent_str, "Product sweep of:")
    elseif isa(sf.iterator, Base.Iterators.Zip)
        print(io, indent_str, "Tandem sweep of:")
    elseif isa(sf.iterator, SerialSweep)
        print(io, indent_str, "Serial sweep of:")
    end
    for it in (isa(sf.iterator, Base.Iterators.Zip) ? sf.iterator.is : sf.iterator.iterators)
        println(io)
        show(IOContext(io, :indent => indent + 2), it)
    end
end


"""
    split_axes(sweep, axes)

Advanced users of sweeps may want to extract large `ProductSweep` runs into
"inner" and "outer" sweeps; this allows e.g. running an external simulator
within a loop over the "outer" sweeps, while allowing the external simulator
to run a limited subset of the sweeps internally.  Note that this only works
on `ProductSweep` objects for now, and that the inverse of this operation is
`ProductSweep(outer, inner)`.  `axes` should be some kind of container that
yields `Symbol`s, such as a `Vector{Symbol}`.

Example:

    outer_sweep, inner_sweep = split_axes(ProductSweep(A=1:10, B=1:8, C=1:6, D=1:4), [:A, :C])
    @test sweepvars(outer_sweep) == Set([:B, :D])
    @test length(outer_sweep) == 32
    @test sweepvars(inner_sweep) == Set([:A, :C])
    @test length(inner_sweep) == 60
"""
function split_axes(sf::SweepFlattener, axes)
    if !isa(sf.iterator, Base.Iterators.ProductIterator)
        throw(ArgumentError("split_axes only works with ProductSweep objects!"))
    end

    # Find the iterator that corresponds to the given axis, or error out
    function find_axis_iterator_idx(axis_name, iterators)
        for (idx, it) in enumerate(iterators)
            if it.selector == axis_name
                return idx
            end
        end
        throw(ArgumentError("Unable to find product axis matching '$(axis_name)'"))
    end

    # Get indices for each axis we're extracting
    inner_idxs = Int[]
    for axis in axes
        push!(inner_idxs, find_axis_iterator_idx(axis, sf.iterator.iterators))
    end

    # Slice them out:
    inner_iterators = sf.iterator.iterators[inner_idxs]
    outer_iterators = sf.iterator.iterators[(1:length(sf.iterator.iterators)) .∉ (inner_idxs,)]

    # Create new ProductSweeps with new selections of iterators
    inner_ps = SweepFlattener(Base.Iterators.product(inner_iterators...))
    outer_ps = SweepFlattener(Base.Iterators.product(outer_iterators...))

    return outer_ps, inner_ps
end


expand(x::Tuple{Symbol,Any}) = tuple(x)
function expand(xs)
    rets = Tuple{Symbol,Any}[]
    for v in xs
        append!(rets, expand(v))
    end
    return tuple(sort(rets, by = ((k, v),) -> k)...)
end

function Base.iterate(sf::SweepFlattener{T}, state...) where {T}
    next = iterate(sf.iterator, state...)
    if next === nothing
        return nothing
    end
    return (expand(next[1]), next[2])
end


"""
    Sweep

Provides a 1-dimensional "sweep" over a given circuit property.  To denote the
the property, you can specify either a Symbol, or an `@optic` selector from the
`Accessors` package.  The swept values can be any kind of iterable, but note
that only `AbstractRange`s will not be immediately expanded.

To combine multiple Sweep objects together, see the combining helper types:

- [`ProductSweep`](@ref)
- [`TandemSweep`](@ref)
- [`SerialSweep`](@ref)

Examples:

    # Argument usage
    Sweep(:R1, 0.1:0.1:1.0)
    Sweep(:R1 => 0.1:0.1:1.0)

    # kwargs usage
    Sweep(R1 = 0.1:0.1:1.0)

    # Nested properties can be accessed via var-strings:
    Sweep(var"a.b" = 1:10)
"""
struct Sweep{T}
    selector::Symbol
    values::Union{AbstractRange{T},Vector{T}}
end

# If someone provided some weird iterable (neither an `AbstractRange`
# nor a `Vector`), just `collect` it immediately.
Sweep(selector::Symbol, values) = Sweep(selector, collect(values)[:])

# Allow using a `Pair`
Sweep((selector, values)::Pair) = Sweep(selector, values)

# kwargs usage
function Sweep(;kwargs...)
    # Only support a single mapping at a time
    if length(kwargs) != 1
        throw(ArgumentError("`Sweep` takes a single variable at a time!"))
    end
    (selector, values) = only(pairs(kwargs))
    return Sweep(selector, values)
end

function Base.:(==)(a::Sweep, b::Sweep)
    return a.selector == b.selector &&
           a.values == b.values
end

function Base.show(io::IO, s::Sweep)
    indent = get(io, :indent, 0)
    indent_str = " " ^ indent
    prefix = indent > 0 ? "- " : "Sweep of "
    if length(s.values) > 1
        minval = minimum(s.values)
        maxval = maximum(s.values)
        print(io, indent_str, prefix, "$(s.selector) with $(length(s.values)) values over [$(minval) .. $(maxval)]")
    else
        print(io, indent_str, prefix, "$(s.selector) set to $(s.values[1])")
    end
end

# Nothing to flatten if it's just a single Sweep
SweepFlattener(s::Sweep) = s


# These are the kinds of things that our Sweep API generates;
# if someone passes a `ProductSweep` into another `ProductSweep`,
# we don't want to try and make a `Sweep` off of that.
const SweepLike = Union{Sweep,SweepFlattener}

# If we're already a Sweep... don't do anything
Sweep(x::SweepLike) = x

# Allow iteration over this `Sweep`, returning `(v.selector, element)`
function Base.iterate(v::Sweep, state...)
    sub_it = Base.iterate(v.values, state...)
    if sub_it === nothing
        return sub_it
    end
    return (((v.selector, sub_it[1]),), sub_it[2])
end
Base.length(v::Sweep) = Base.length(v.values)
Base.size(v::Sweep) = size(v.values)
Base.size(v::Sweep, d::Integer) = get(size(v), d, 1)
Base.IteratorSize(v::Sweep) = Base.IteratorSize(v.values)
sweep_example(v::Sweep) = first(v)

"""
    sweepvars(sweep)

Return the full `Set` of variables (in the form of symbols) that can be set when
iterating over the given sweep object.
"""
sweepvars(v::Sweep) = Set([v.selector])
sweepvars(v::Tuple{}) = Set{Symbol}()
sweepvars(vs...) = !isempty(vs) ? union(sweepvars.(vs)...) : Set{Symbol}()

"""
    ProductSweep(args...)

Takes the given `Sweep` specifications and produces the cartesian product of
the individual inputs:

    ProductSweep(R1 = 1:4, R2 = 1:4, R3 = 1:4)

    ProductSweep(:R1 => 1:4, :R2 => 1:4)
"""
function ProductSweep(args...; kwargs...)
    # Special-case; if we're given only one thing, just turn it into a `Sweep`
    if length(args) + length(kwargs) == 1
        return Sweep(collect(args)...; collect(kwargs)...)
    end
    return SweepFlattener(product(Sweep.(collect(args))..., Sweep.(collect(kwargs))...))
end
sweepvars(pi::Base.Iterators.ProductIterator) = sweepvars(pi.iterators...)

"""
    TandemSweep(args...)

Takes the given `Sweep` specifications and produces an iterator that emits
each input in tandem.  Note that all inputs must have the same length!

    TandemSweep(R1 = 1:4, R2 = 1:4)
"""
function TandemSweep(vs::Sweep...)
    # Special-case; if we're given only one thing, just turn it into a `Sweep`
    if length(vs) == 1
        return vs[1]
    end
    lengths = length.(vs)
    if !all(lengths .== lengths[1])
        throw(ArgumentError("TandemSweep requires all sweeps be of the same length!"))
    end
    return zip(vs...)
end
TandemSweep(args...; kwargs...) = SweepFlattener(TandemSweep(Sweep.(collect(args))..., Sweep.(collect(kwargs))...))
sweepvars(ts::Base.Iterators.Zip) = sweepvars(ts.is...)

"""
    SerialSweep(args...)

Takes the given `Sweep` specifications and produces an iterator that emits
each input in serial.

    SerialSweep(R1 = 1:4, R2 = 1:4)
"""
struct SerialSweep
    iterators::Vector
    # We keep track of our full set of variables so that we can always emit (v1 = nothing, v2 = x, v3 = nothing, v4 = nothing)
    vars::Set{Symbol}

    function SerialSweep(args...; kwargs...)
        iterators = [Sweep.(collect(args))..., Sweep.(collect(kwargs))...]
        # Special-case; if we're given only one thing, just turn it into a `Sweep`
        if length(iterators) == 1
            return iterators[1]
        end
        vars = sweepvars(iterators...)
        return SweepFlattener(new(iterators, vars))
    end
end

function Base.iterate(ss::SerialSweep, state = (1,))
    it_idx = state[1]
    if it_idx > length(ss.iterators)
        return nothing
    end
    next = iterate(ss.iterators[it_idx], Base.tail(state)...)
    while next === nothing
        it_idx += 1
        if it_idx > length(ss.iterators)
            return nothing
        end
        next = iterate(ss.iterators[it_idx])
    end
    mappings = Dict{Symbol,Any}(v => nothing for v in ss.vars)
    for (var, val) in expand(next[1])
        mappings[var] = val
    end
    return (tuple(((var, val) for (var, val) in mappings)...), (it_idx, next[2]))
end
Base.length(ss::SerialSweep) = sum(length(it) for it in ss.iterators; init=0)
Base.size(ss::SerialSweep) = (length(ss),)
Base.size(ss::SerialSweep, d::Integer) = get(size(ss), d, 1)
sweepvars(ss::SerialSweep) = sweepvars(ss.iterators...)

"""
    sweepify(obj)

Converts convenient shorthands for sweep specifications from julia syntax to
actual [`Sweep`](@ref) objects.  `NamedTuple` objects are turned into
[`SerialSweep`](@ref)'s, vectors are turned into [`SerialSweep`](@ref)'s.

    sweepify([(r1 = 1:10, r2 = 1:10), (r3 = 1:10, r4=1:10)])
"""
sweepify(x::AbstractArray) = SerialSweep(sweepify.(x)...)
function sweepify(x::NamedTuple)
    ProductSweep(;(k => v for (k, v) in pairs(x))...)
end
sweepify(x::SweepLike) = x
sweepify(x) = Sweep(x)


"""
    CircuitSweep

Provides a multi-dimensional sweep over sets of variables defined within a
circuit.  Can take in either a circuit struct type, or an `IRODESystem`.  When
iterated over, returns the altered circuit object.  Parameter specification can
be done with simple `Symbol`'s, or with more complex `@optic` values from the
`Accessors` package.

Examples:

    # You can construct the CircuitSweep object off of either a circuit
    # or an `IRODESystem` that was constructed off of a circuit:
    cs = CircuitSweep(RCCircuit, ProductSweep(:R1 => 0.1:0.1:1.0, :R2 => [1.0, 10.0, 100.0]))

    # You can also use your own `sys` object:
    sys = CircuitIRODESystem(RCCircuit)
    cs = CircuitSweep(RCCircuit, sys, ProductSweep(R1 = 0.1:0.1:1.0, R2 = [1.0, 10.0, 100.0]))

    # Once you have the `CircuitSweep` object, you can iterate over it:
    for circuit in cs
        @show circuit
        ...
    end

    # You can also pass it off to `dc!.()`:
    sols = dc!.(cs)

    # Note that if you want to index into the solution objects, you should
    # use the `cs.sys` property, as you need to use the same `sys` as was
    # used to run the actual simulations:
    Vout = sols[1][cs.sys.Vout]
"""
struct CircuitSweep{T}
    circuit::T
    sys::IRODESystem
    iterator::SweepLike

    function CircuitSweep(circuit::T, sys::IRODESystem, iterator::SweepLike) where {T}
        # Verify that the given `iterator` object works:
        if !applicable(iterate, iterator)
            throw(ArgumentError("Must give some kind of iterator!"))
        end

        # TODO: Add ability to verify that all optics are applicable to the given circuit
        # We used to do this via `hasproperty(circuit_inst, optic)`, but that doesn't
        # work for general ParamSim objects now that they can be applied to functions
        # via that lens parameter.
        return new{T}(
            circuit,
            sys,
            iterator,
        )
    end
end

# Convenience function to compile a circuit to a ParamSim IRODESystem
function CircuitSweep(circuit, iterator::SweepLike; debug_config = (;))
    sys = CircuitIRODESystem(ParamSim(circuit; sweep_example(iterator)...); debug_config)
    return CircuitSweep(circuit, sys, iterator)
end

Base.length(cs::CircuitSweep) = length(cs.iterator)
Base.size(cs::CircuitSweep) = size(cs.iterator)
Base.size(cs::CircuitSweep, d::Integer) = get(size(cs), d, 1)
Base.IteratorSize(cs::CircuitSweep) = Base.IteratorSize(cs.iterator)
function _iterate_alter(cs::CircuitSweep, next)
    if next === nothing
        return nothing
    end

    # Apply the parameters to our circuit
    new_circuit = ParamSim(cs.circuit; next[1]...)

    # Return the circuit and the state for future iteration
    return (new_circuit, next[2])
end
Base.iterate(cs::CircuitSweep, state...) = _iterate_alter(cs, iterate(cs.iterator, state...))
sweepvars(cs::CircuitSweep) = sweepvars(cs.iterator)

function dc!(circ; debug_config = (;), kwargs...)
    if !isa(circ, AbstractSim)
        circ = CedarSim.DefaultSim(circ)
    end
    dc!(CircuitIRODESystem(circ; debug_config), circ; kwargs...)
end
function dc!(sys::IRODESystem, circ=DAECompiler.arg1_from_sys(sys); kwargs...)
    prob = DAEProblem(sys, nothing, nothing, (0., 1.), circ, initializealg=CedarDCOp())
    dc!(prob; kwargs...)
end
dc!(prob::DAEProblem; kwargs...) = init(prob, DFBDF(autodiff=false); kwargs...)
dc!(cs::CircuitSweep; kwargs...) = dc!.(cs.sys, cs; kwargs...)

function tran!(circ, tspan, du=nothing, u=nothing; debug_config = (;), kwargs...)
    sim = CedarSim.DefaultSim(circ)
    sys = CircuitIRODESystem(circ; debug_config)
    prob = DAEProblem(sys, du, u, tspan, sim, initializealg=CedarDCOp())
    tran!(prob; kwargs...)
end
tran!(prob::DAEProblem; kwargs...) = solve(prob, IDA(); kwargs...)
tran!(cs::CircuitSweep; kwargs...) = tran!.(cs.sys, cs; kwargs...)
function tran!(circ::ParsedCircuit, tspan=find_default_tspan(circ); debug_config=(;), kwargs...)
    sim = CedarSim.DefaultSim(circ)
    sys = CircuitIRODESystem(circ; debug_config)
    prob = DAEProblem(sys, nothing, nothing, tspan, sim, initializealg=CedarDCOp())
    tran!(prob; kwargs...)
end

DiffEqBase.solve(ps::ParsedCircuit) = tran!(ps)

# Helper function of `first()` that passes through `nothing`
first_or_nothing(x) = first(x)
first_or_nothing(::Nothing) = nothing

function Base.Broadcast.broadcasted_kwsyntax(::typeof(dc!), sys::IRODESystem, sims, du0=nothing, u0=nothing; kwargs...)
    prob = DAEProblem(sys, first_or_nothing(du0), first_or_nothing(u0), (0., 1.), first(sims), initializealg=CedarDCOp())
    broadcast(sims, du0, u0) do sim, this_du0, this_u0
        if u0 !== nothing || du0 !== nothing
            @assert du0 !== nothing && u0 !== nothing
            prob′ = remake(prob, p=sim, u0=this_u0, du0=this_du0)
        else
            prob′ = remake(prob, p=sim)
        end
        dc!(prob′; kwargs...)
    end
end

function Base.Broadcast.broadcasted(::typeof(dc!), sys::IRODESystem, circs, du0=nothing, u0=nothing; kwargs...)
    return Base.Broadcast.broadcasted_kwsyntax(dc!, sys, circs, du0, u0; kwargs...)
end

function Base.Broadcast.broadcasted_kwsyntax(::typeof(tran!), sys::IRODESystem, tspan, circs, du0=nothing, u0=nothing; kwargs...)
    prob = DAEProblem(sys, first_or_nothing(du0), first_or_nothing(u0), tspan, DefaultSim(first(circs)), initializealg=CedarDCOp())
    broadcast(circs, du0, u0) do circ, this_du0, this_u0
        if u0 !== nothing || du0 !== nothing
            @assert du0 !== nothing && u0 !== nothing
            prob′ = remake(prob, p=DefaultSim(circ), u0=this_u0, du0=this_du0)
        else
            prob′ = remake(prob, p=DefaultSim(circ))
        end
        tran!(prob′; kwargs...)
    end
end
function Base.Broadcast.broadcasted(::typeof(tran!), sys::IRODESystem, circs, du0=nothing, u0=nothing; kwargs...)
    return Base.Broadcast.broadcasted_kwsyntax(tran!, tspan, sys, circs, du0, u0; kwargs...)
end



# Base case; a single parameter mapped on certain values:
function find_param_ranges(it::CedarSim.Sweep, param_ranges)
    if !haskey(param_ranges, it.selector)
        param_ranges[it.selector] = Tuple[]
    end
    push!(param_ranges[it.selector], (minimum(it.values), maximum(it.values), length(it.values)))
end
find_param_ranges(sf::CedarSim.SweepFlattener, param_ranges) = find_param_ranges(sf.iterator, param_ranges)
function find_param_ranges(it, param_ranges)
    children(it::Base.Iterators.Zip) = it.is
    children(it::Base.Iterators.ProductIterator) = it.iterators
    children(it::SerialSweep) = it.iterators
    for child in children(it)
        find_param_ranges(child, param_ranges)
    end
end

function collapse_ranges(ranges::Vector)
    total_min, total_max, total_len = ranges[1]
    for range in ranges[2:end]
        new_min, new_max, new_len = range
        total_min = min(new_min, total_min)
        total_max = max(new_max, total_max)
        total_len += new_len
    end
    return (total_min, total_max, total_len)
end

"""
    find_param_ranges(params)

Given a sweep specification, return a simplified view of the range of parameter
exploration and number of points within that range that will be explored.  This
loses all detail of product vs. serial vs. tandem, etc... but gives a rough idea
of the bounds along each dimension.
"""
function find_param_ranges(params)
    param_ranges = Dict{Symbol,Vector{Tuple}}()
    find_param_ranges(params, param_ranges)
    return Dict(name => collapse_ranges(ranges) for (name, ranges) in param_ranges)
end
