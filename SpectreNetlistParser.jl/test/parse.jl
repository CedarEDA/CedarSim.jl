using SpectreNetlistParser
using SpectreNetlistParser.SpectreNetlistCSTParser: EXPR, Analysis, reducedcontent
using Test

var = """
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

subckt s2 D0 D1 PD PIX_OUT VDD VLOG VSS
    C13 (VLOG PD) capacitor c=c_vlog_vpd
    C9 (VDD D1) capacitor c=c_vd1_vdd
    C8 (VLOG VSS) capacitor c=c_vlog_vss
    C15 (D1 VSS) capacitor c=c_vd1_vss
ends s2

save 7
save Q4:currents
save Q1:1
save M2.d:displacement
save Q3:currents M1:all
save F4.S1.BJT3:oppoint
"""

function check_roundtrip(var; offset=0)
    s1 = SpectreNetlistParser.parse(var; offset)
    s1_str = reducedcontent(s1)
    s2 = SpectreNetlistParser.parse(s1_str)
    s2_str = reducedcontent(s2)
    @test s1_str == s2_str
end

function check_roundtrip_file(file)
    s1 = SpectreNetlistParser.parsefile(file)
    s1_str = reducedcontent(s1)
    s2 = SpectreNetlistParser.parse(s1_str; fname=file)
    s2_str = reducedcontent(s2)
    @test s1_str == s2_str
end

check_roundtrip(var)

with_offset = """
This is
some initial
garbage
simulator lang=spectre
r2 (1 0) resistor r=1k
"""
check_roundtrip(with_offset; offset=29)

with_language_swap = """
simulator lang=spectre
r2 (1 0) resistor r=1k
simulator lang=spice
r1 1 0 1k
v1 1 0 1
simulator lang=spectre
r3 (1 0) resistor r=1k
"""
check_roundtrip(with_language_swap)

function test_examples(; verbose=false)
    for file in readdir(joinpath(@__DIR__, "examples"); join=true)
        verbose && @info "Spectre parsing $(repr(file))"
        check_roundtrip_file(file)
    end
end
test_examples()

analysis = "FindNoise (out gnd) noise start=1 stop=1MHz"
ast = SpectreNetlistParser.parse(analysis)
@test ast.expr.form.stmts[1] isa EXPR{Analysis}

wave = """
parameters name=[ 0 var +3 -2 (inparen) var \
(inparen+1u) (var-(1-x)) ]
"""
ast = SpectreNetlistParser.parse(wave)
# Check the expected number of items in the Array
array = ast.expr.form.stmts[1].params[1].val
@test length(array.form.items) == 8

# Test error
faulty = """

parameters a== 1
"""
e = try SpectreNetlistParser.parse(faulty)
catch g
    g
end
str = sprint(io -> Base.showerror(io, e))
@test occursin("SpectreParser error at line 2:\n  parameters a== 1", str)

nothing
