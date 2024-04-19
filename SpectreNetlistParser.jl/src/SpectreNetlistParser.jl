module SpectreNetlistParser

abstract type AbstractTerminal end
isunit(x) = false
istitle(x) = false

include("Tries.jl")
include("LineNumbers.jl")
include("EXPRS.jl")
include("RedTree.jl")
include("utils.jl")

struct SrcFile
    path::Union{Nothing, String}
    contents::Union{String, IOBuffer}
    lineinfo::LineNumbers.SourceFile

    function SrcFile(path, contents, lineinfo)
        if isa(path, AbstractString)
            path = string(path)
        end
        if isa(contents, AbstractString)
            contents = string(path)
        end
        return new(path, contents, lineinfo)
    end
end

include("SPICE/SPICENetlistParser.jl")

include("tokenize/SpectreNetlistTokenize.jl")
include("parse/SpectreNetlistCSTParser.jl")

using .SpectreNetlistCSTParser: parse, parsefile

using SnoopPrecompile
@precompile_all_calls begin
    parsefile(joinpath(@__DIR__, "../test/examples/inv1x2.scs"))
    SPICENetlistParser.parsefile(joinpath(@__DIR__, "../test/examples/RLC_test.cir"))

    spice = """
    .TITLE THIS IS A TILE WITH ¤%&/)/& stuff
    Vv-_-{} A B 0
    * MOSFET
    .GLOBAL VDD NET1
    .MODEL BJT_modName NPN (BF=val)
    Rname N1 N2 0.1 \$ comment
    .param freq = 1Meg
    *comment
        *comment
    .lib 'with spaces/sm141064.ngspice' nmos_6p0_t
    .lib sm141064.ngspice nmos_6p0_t
    .include ./foo
    .include './foo.bar'
    .param r_l='s*(r_length-2*r_dl)'
    Q1 Net-_Q1-C_ Net-_Q1-B_ 0 BC546B
    X1 v+ v- r={a-b} f-o-o=1
    .ic v( m_tn4:d )=  1.225e-08
    V1 vin 0 SIN (0, 1, 1k)
    R1 (a b) r=\"foo+bar\"
    .MEAS TRAN res1 FIND V(out) AT=5m
    .tran 1ns 60ns
    .DC Vds 0 5 0.5 Vgs 0 5 1
    .GLOBAL VDD NETGLOBAL
    Rname N1 N2 0.5
    *.MODEL BJT_modName NPN (BF=val IS=val VAF=val) \$ comment
    .param  l = 1 w = 1 ad = 0 as = 0 pd = 0 ps = 0 nrd = 0 nrs = 0 sa = 0 sb = 0 sd = 0 mult = 1 nf = 1.0
    + toxe = {1.12565e-08+sky130_fd_pr__pfet_g5v0d10v5__toxe_slope_spectre*(1.12565e-08*(sky130_fd_pr__pfet_g5v0d10v5__toxe_slope/sqrt(l*w*mult)))}

    * msky130_fd_pr__pfet_g5v0d10v5 d g s b sky130_fd_pr__pfet_g5v0d10v5__model l = {l} w = {w} ad = {ad} as = {as} pd = {pd} ps = {ps} nrd = {nrd} nrs = {nrs} sa = {sa} sb = {sb} sd = {sd} nf = {nf}
    * DC IV MOS Parameters

    .LIB ff
    .lib 'sm141064.ngspice' nmos_3p3_f
    .lib sm141064.ngspice fets_mm
    .ENDL
    .include foo.spice

    q0 c b e s  vpnp_10x10  dtemp=dtemp
    q0 c b e vpnp_0p42x10  dtemp=dtemp

    .param mc_sig_vth2 = agauss(0, 1, 3)
    .END
    """
    SPICENetlistParser.parse(spice)
    spectre = """
    parameters p1=23pf p2=.3 p3 = 1&2~^3 p4 = true && false || true p5 = M_1_PI * 3.0
    Vdd (vdd   gnd)   vsource dc=p1+p1*p2 stop = 0.3MHz values=[-50 0 50 100 125]
    MyAcct1 info what=inst extremes=yes // fdsfdsf
    tran tran stop=30n infotimes=[10n 25n] infoname=opinfo
    ParamChk check what=inst // fdsfdsf
    Mychecklimit1 checklimit disable=["assert2" "assert5"]
    Examp options rawfmt=psfbin audit=brief temp=30 \
        save=lvlpub nestlvl=3 rawfile="%C:r.raw" useprobes=no
    Quiet set narrate=no error=no info=no
    Name shell parameter=value
    change2 alter param=temp value=0
    ag1 altergroup {
        parameters p1=1
        model myres resistor r1=1e3 af=p1
        model mybsim bsim3v3 lmax=p1 lmin=3.5e-7
    }
    subckt twisted
        parameters zodd=50 zeven=50 veven=1 vodd=1 len=0 // fdsfdsf
        odd (p1 n1 p2 n2) tline z0=zodd vel=vodd len=len
        tf1a (p1 0 e1 c1) transformer t1=2 t2=1
        tf1b (n1 0 c1 0) transformer t1=2 t2=1
        even (e1 0 e2 0) tline z0=zeven vel=veven len=len
        tf2a (p2 0 e2 c2) transformer t1=2 t2=1
    ends

    subckt s1
    parameters p1=(a+b)      // subcircuit
        r1 (1 0) resistor r=p1      // another simple expression // fdsfdsf
        r2 (1 0) resistor r=p2*p2   // a binary multiply expression
        r3 (1 0) resistor r=(p1+p2)/p3      // a more complex expression
        r4 (1 0) resistor r=sqrt(p1+p2)     // an algebraic function call
        r5 (1 0) resistor r=3+atan(p1/p2) //a trigonometric function call
        r6 (1 0) RESMOD r=(p1 ? p4+1 : p3)  // the ternary operator
    ends

    odd (s1.r1 n1 s1.r1.r2.r3 n2) tline z0=zodd vel=vodd len=len

    //  a model statement, containing expressions
    model RESMOD resistor tc1=p1+p2 tc2=sqrt(p1*p2)
    //  some expressions used with analysis parameters // fdsfdsf
    time_sweep tran start=0 stop=(p1+p2)*50e-6 // use 5*50e-6 = 150 us
    //  a vector of expressions (see notes on vectors below)
    dc_sweep dc param=p1 values=[0.5 1 +p2 (sqrt(p2*p2)) ]  // sweep p1

    real myfunc( real a, real b ) {
        return a+b*2+sqrt(a*sin(b));
    }
    if ( area < 100e-12 ) {
        npn_mod (c b e s) npn10x10  // 10u * 10u, inline device
    } else if ( area < 400e-12 ) {
        npn_mod (c b e s) npn20x20  // 20u * 20u, inline device
    } else if ( area < 400e-12 ) {
        npn_mod (c b e s) npn20x20  // 20u * 20u, inline device
    } else {
        npn_mod (c b e s) npn_default       // 5u * 5u, inline device
    }

    inline subckt s1 (a b) // "s1" is name of subcircuit
        parameters l=1u w=2u
        s1 (a b 0 0) mos_mod l=l w=w// "s1" is "inline" component
        cap1 (a 0) capacitor c=1n
        cap2 (b 0) capacitor c=1n
    ends s1


    save 7
    save Q4:currents
    save Q1:1
    save M2.d:displacement
    save Q3:currents M1:all
    save F4.S1.BJT3:oppoint
    """
    parse(spectre)

end

end
