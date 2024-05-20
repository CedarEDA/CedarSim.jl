# Just for demonstration. TODO: Replace by device in the full package
# Positive currents flow out of devices

using DAECompiler
using DAECompiler.Intrinsics: variable, equation!, observed!, singularity_root!, sim_time, GenScope
using ExprTools: splitdef
using Base.Meta: isexpr

export SimpleResistor, SimpleCapacitor, SimpleInductor, VoltageSource, VariableVoltageSource, Gnd,
    gnd, @subckt, SubCircuit

abstract type CircuitElement end
abstract type SPICEDevice <: CircuitElement; end

struct SubCircuit{T} <: CircuitElement
    ckt::T
end
spicesymbol(::SubCircuit) = "X"

function (X::SubCircuit)(args...; dscope=defaultscope(X))
    @with debug_scope=>dscope X.ckt(args...)
end

macro subckt(args...)
    if length(args) == 1
        body, = args
        parts = splitdef(body)
        push!(parts[:kwargs], Expr(:kw, :dscope,
            :($(DScope)($(debug_scope)[], Base.typename($(Core.Argument(1))).name))))
        parts[:body] = quote
            @Base.with $(debug_scope)=>dscope begin
                $(parts[:body])
            end
        end
        return esc(quote
            @Base.__doc__ $(combinedef(parts))
            function (name::String)($(parts[:name]))
                Named($(Core.Argument(2)), name)
            end
        end)
    elseif length(args) == 2
        (name, body) = args
        return esc(quote
            @Base.with $(debug_scope)=>$(DScope)($(debug_scope)[], $(QuoteNode(name))) $(body)
        end)
    end
end

struct SimpleResistor <: SPICEDevice
    r::DefaultOr{Float64}
    rsh::Float64
    w::Float64
    l::Float64
    narrow::Float64
    short::Float64
    ϵthermal::Float64
end
SimpleResistor(r) = SimpleResistor(;r)
SimpleResistor(;r=mkdefault(1), rsh=50, wdef=1e-6, w=wdef, l=wdef, narrow=0, short=0, ϵthermal=0, kwargs...) = SimpleResistor(r, undefault(rsh), undefault(w), undefault(l), undefault(narrow), undefault(short), undefault(ϵthermal))
spicesymbol(::SimpleResistor) = "R"

defaultscope(dev::Symbol) = GenScope(debug_scope[], dev)
defaultscope(dev::Union{SPICEDevice, SubCircuit}) =  defaultscope(Symbol(spicesymbol(dev)))

function (R::SimpleResistor)(A, B; dscope=defaultscope(R))
    res = if isdefault(R.r)
        R.rsh*(R.l - R.short)/(R.w - R.narrow)
    else
        undefault(R.r)
    end
    k = 1.380649e-23
    T = undefault(spec[].temp)+273.15
    pwr = 4*k*T/res
    branch!(dscope, A, B) do V, I
        I - V/res - VerilogAEnvironment.white_noise(dscope, pwr, :thermal)
    end
end

struct NonlinearResistor{F} <: CircuitElement
    iv::F
end
function (R::NonlinearResistor)(A, B; dscope=GenScope(debug_scope[], "NR"))
    branch!(dscope, A, B) do V, I
        I - R.iv(V)
    end
end

struct OpenCircuit{F} <: CircuitElement end
function (::OpenCircuit)(A, B; name)
    return nothing
end
struct ShortCircuit{F} <: CircuitElement end
function (::ShortCircuit)(A, B; dscope=nothing)
    branch!(A, B) do V, I
        return V
    end
end

struct SimpleCapacitor{T} <: SPICEDevice
    capacitance::T
end
SimpleCapacitor(;c=1, kwargs...) = SimpleCapacitor(undefault(c))
spicesymbol(::SimpleCapacitor) = "C"

function (C::SimpleCapacitor)(A, B; dscope=defaultscope(C))
    branch!(dscope, A, B) do V, I
        I - ddt(V)*C.capacitance
    end
end

struct NonlinearCapacitor{F} <: CircuitElement
    qv::F
end
function (C::NonlinearCapacitor)(A, B; dscope=GenScope(debug_scope[], "NC"))
    branch!(dscope, A, B) do V, I
        Q = variable(dscope(:Q))
        equation!(Q - C.qv(V), sc)
        return I - ddt(Q)
    end
end

struct SimpleInductor{T} <: SPICEDevice
    inductance::T
end
SimpleInductor(;l) = SimpleInductor(undefault(l))
spicesymbol(::SimpleInductor) = "L"

function (L::SimpleInductor)(A, B; dscope=defaultscope(L))
    branch!(dscope, A, B) do V, I
        return V - ddt(I)*L.inductance
    end
end

struct SimpleDiode <: CircuitElement
    AREA::Float64
    BV::Float64
    BVJ::Float64
    CJO::Float64
    EG::Float64
    FC::Float64
    IBV::Float64
    IS::Float64
    KF::Float64
    M::Float64
    N::Float64
    RS::Float64
    TNOM::Float64
    TT::Float64
    VJ::Float64
    XTI::Float64

    function SimpleDiode(;
        AREA= 1.0,
        BV = 3.40282e+38,
        BVJ = 3.40282e+38,
        CJO = 0.0,
        EG = 1.11,
        FC = .5,
        IBV = .001,
        IS = 1e-14,
        KF = 0.,
        M = .5,
        N = 1.,
        RS = 0.,
        TNOM = 27.,
        TT = 0.,
        VJ = 1.,
        XTI = 3.,
        kwargs...)
        AREA,BV,BVJ,CJO,EG,FC,IBV,IS,KF,M,N,RS,TNOM,TT,VJ,XTI = Float64.(undefault.((AREA,BV,BVJ,CJO,EG,FC,IBV,IS,KF,M,N,RS,TNOM,TT,VJ,XTI)))
        for param in (AREA, EG)
            @assert param > 0
        end
        for param in (BV, BVJ, CJO, IBV, IS, M, N, RS, TT, XTI)
            @assert param >= 0
        end
        @assert VJ != 0
        new(AREA,BV,BVJ,CJO,EG,FC,IBV,IS,KF,M,N,RS,TNOM,TT,VJ,XTI)
    end
end

function (D::SimpleDiode)(A, B; dscope=defaultscope(D))
    return branch!(dscope, A, B) do V, I
        Vd = V
        AREA = D.AREA
        BV = D.BV
        BVJ = D.BVJ
        CJO = D.CJO
        EG = D.EG
        FC = D.FC
        IBV = D.IBV
        IS = D.IS
        KF = D.KF
        M = D.M
        N = D.N
        RS = D.RS
        TNOM = D.TNOM
        TT = D.TT
        VJ = D.VJ
        XTI = D.XTI
        #aliases
        MJ = M
        PB = VJ
        CJ0 = CJO

        # hard coded for now
        GMIN = 1e-12
        PJ = 0.
        CJP = 0.
        PHP = 1.
        FCS = .5
        JSW = 0.
        MJSW = 1/3
        IKF = Inf
        IK = IKF
        IKR = Inf

        #hard coded cause universe
        q = -1.602176634e-19 # electron charge
        k = 1.380649e-23

        T = 300
        AREAeff = AREA*M
        PJeff = PJ*M
        ISeff = IS*AREAeff+JSW*PJeff
        IBVeff =IBV*AREAeff
        IKeff = IK*AREAeff
        IKReff = IKR*AREAeff
        CJeff = CJO*AREAeff
        CJPeff = CJP*PJeff

        NkT = N*k*T
        Ibdwn = ISeff*expm1(-q*BV/NkT)
        if IBVeff < Ibdwn
            IBVeff = Ibdwn
            BVeff = BV
        else
            BVeff = BV - NVt * ln(IBVeff/Ibdwn)
        end

        if Vd >= -3*NkT/q
            Id = ISeff*expm1(q*Vd/NkT)
        elseif -BVeff < Vd
            Id = -ISeff*(1+(3*NkT/(q*Vd*ℯ))^3)
        else
            Id = -ISeff*exp(-q*(BVeff+Vd)/NkT)
        end
        Id += Vd*GMIN

        if Vd >= -3*N*k*T/q
            Ideff = Id/(1+sqrt(Id/IKeff))
        else -BVeff < Vd
            Ideff = Id/(1+sqrt(Id/IKReff))
        end
        # capacitance
        Cdiffuse = TT*ddt(Ideff)/ddt(Vd)
        if Vd < FC*VJ
            Cdeplete_bw = CJeff*(1-Vd/VJ)^-MJ
        else
            Cdeplete_bw = CJeff*(1-FC*(1+MJI)+MJ*Vd/VJ)/(1-FC)^(1+MJ)
        end
        if Vd < FCS*PHP
            Cdeplete_sw = CJeff*(1-Vd/PHP)^-MJSW
        else
            Cdeplete_sw = CJeff*(1-FCS*(1+MJSW)+MJSW*Vd/PHP)/(1-FCS)^(1+MJSW)
        end
        C = Cdiffuse + Cdeplete_bw + Cdeplete_sw

        return -I + Id + ddt(V)*C
    end
end

# This only saves the value during the analysis
struct VoltageSource{T} <: SPICEDevice
    dc::T
    tran::T
    ac::Complex{T}
end
function VoltageSource(;type=nothing, dc=nothing, ac=0.0+0.0im, tran=nothing)
    dc = something(dc, tran, 0.0)
    tran = something(tran, dc)
    dc, tran, acre, acim = promote(dc, tran, reim(ac)...)
    VoltageSource(dc, tran, acre+im*acim)
end

spicesymbol(::VoltageSource) = "V"

function (VS::VoltageSource)(A, B; dscope=defaultscope(VS))
    return branch!(dscope, A, B) do V, I
        if sim_mode[] === :dcop
            Vab = VS.dc
        elseif sim_mode[] === :ac
            ac = spec[].ϵω * abs(VS.ac) # TODO phase
            Vab = VS.dc + ac
        else
            Vab = VS.tran
        end
        return V - Vab
    end
end

struct Gnd <: CircuitElement; end
gnd() = (g = net(); Gnd()(g); g)

function (R::Gnd)(A; dscope=debug_scope[])
    # In general, spice simulators omit the KCL for ground nets, but we don't
    # know in advance which nets are ground. We don't have that ability here,
    # but adding an unconstrained variable has the same effect.
    kcl!(A, variable())
    # It's in general permitted to have multiple GND devices on the same net,
    # so use a GenScope here to avoid a warning.
    equation!(A.V - 0, GenScope(dscope, :GND))
end

struct CurrentSource{T} <: SPICEDevice
    dc::T
    tran::T
    ac::Complex{T}
end
function CurrentSource(;type=nothing, dc=nothing, ac=0.0+0.0im, tran=nothing)
    dc = something(dc, Some(tran))
    tran = something(tran, dc)
    dc, tran, acre, acim = promote(dc, tran, reim(ac)...)
    CurrentSource(dc, tran, acre+im*acim)
end

function (IS::CurrentSource)(A, B; dscope=defaultscope(IS))
    return branch!(dscope, A, B) do V, I
        if sim_mode[] === :dcop
            Iab = IS.dc
        elseif sim_mode[] === :ac
            ac = spec[].ϵω * abs(IS.ac) # TODO phase
            Iab = IS.dc + ac
        else
            Iab = IS.tran
        end
        return I - Iab
    end
end

struct vcvs <: SPICEDevice
    gain::Float64
    voltage::Float64
end
vcvs(;gain=1.0, vol=nothing, value=nothing) = vcvs(undefault(gain), something(undefault(vol), undefault(value), 0.0))

function (S::vcvs)(A, B; dscope=defaultscope(S))
    branch!(dscope, A, B) do V, I
        return V - S.voltage
    end
end
function (S::vcvs)(A, B, C, D; dscope=defaultscope(S))
    branch!(dscope, A, B) do V, I
        return V - S.gain*(C.V - D.V)
    end
end

struct vccs <: SPICEDevice
    gain::Float64
    current::Float64
end
vccs(;gain=1.0, cur=nothing, value=nothing) = vccs(undefault(gain), something(undefault(cur), undefault(value), 0.0))

function (S::vccs)(A, B; dscope=defaultscope(S))
    branch!(dscope, A, B) do V, I
        return I - S.current
    end
end
function (S::vccs)(A, B, C, D; dscope=defaultscope(S))
    branch!(dscope, A, B) do V, I
        return I - S.gain*(C.V - D.V)
    end
end

@Base.kwdef struct Switch <: SPICEDevice
    vt::Float64=0.0
    vh::Float64=0.0
    ron::Float64=1.0
    roff::Float64=1e12
    initial::Bool=false
end

function (S::Switch)(A, B, CA, CB; dscope=defaultscope(S))
    cedarerror("Switch device is currently Unimplemented")
end

#=
function (S::Switch)(A, B, CA, CB)
    cmp = variable(DScope(debug_scope[], :cmp))
    CV = CA.V - CB.V
    # During initialisation, bias towards specified ON/OFF state
    if sim_mode[] ∈ (:dcop, :tranop)
        vh = S.initial ? S.vh : -S.vh
    else
        vh = sign(cmp)*S.vh
    end
    vt = S.vt - vh
    diff = CV - vt
    equation!(cmp - diff)
    observed!(vt, DScope(debug_scope[], :vt))
    singularity_root!(diff)
    on = cmp > 0
    R = on ? S.ron : S.roff
    branch!(A, B) do V, I
        I - V/R
    end
end
=#

struct UnimplementedDevice
    params
end
UnimplementedDevice(;kwargs...) = UnimplementedDevice(kwargs)

function (::UnimplementedDevice)(args...; dscope=nothing)
    cedarerror("Unimplemented device")
end
