using CassetteOverlay

struct AliasNet <: AbstractNet
    name::Union{DScope, Nothing}
    kcl!::equation
    V::Float64
    multiplier::Float64
end

struct AliasInterp <: CassetteOverlay.AbstractBindingOverlay{nothing, nothing}
    aliases::Dict{DScope, DScope}
end

function AliasInterp()
    AliasInterp(Dict())
end

function (self::AliasInterp)(::Type{Net}, name::Union{DScope, Nothing}=nothing, multiplier::Float64 = 1.0)
    AliasNet(name, equation(name), 0, multiplier)
end
function (self::AliasInterp)(::Type{Net}, net::AliasNet, multiplier::Float64)
    AliasNet(net.name, net.kcl!, 0, net.multiplier * multiplier)
end

@noinline function (self::AliasInterp)(::typeof(net_alias), net, name)
    dname = DScope(debug_scope[], name)
    self.aliases[dname] = net.name
end

function (self::AliasInterp)(::typeof(with), f, pairs...)
    with(pairs...) do
        self(f)
    end
end

function aliasmap(circ)
    interp = CedarSim.AliasInterp()
    interp(circ)
    interp.aliases
end