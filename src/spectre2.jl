using SpectreNetlistParser
using SpectreNetlistParser: SpectreNetlistCSTParser, SPICENetlistParser
using .SPICENetlistParser: SPICENetlistCSTParser
using .SpectreNetlistCSTParser:
    SpectreNetlistSource
using .SPICENetlistCSTParser:
    SPICENetlistSource
using VerilogAParser
const SNode = SpectreNetlistCSTParser.Node
const VANode = VerilogAParser.VerilogACSTParser.Node
const SC = SpectreNetlistCSTParser
const SP = SPICENetlistCSTParser


using CedarSim
include(joinpath(dirname(pathof(CedarSim)), "spc/interface.jl"))
include(joinpath(dirname(pathof(CedarSim)), "spc/sema.jl"))
include(joinpath(dirname(pathof(CedarSim)), "spc/codegen.jl"))
include(joinpath(dirname(pathof(CedarSim)), "spc/query.jl"))
include(joinpath(dirname(pathof(CedarSim)), "spc/generated.jl"))

#=
ckt = sp"""
    * Inverter test

    Xneg VSS D Q VSS nfet_06v0 W=3.6e-07 L=6e-07
    Xpos VDD D Q VDD pfet_06v0 W=4.95e-07 L=5e-07

    VVDD VDD 0 5.0
    VVSS VSS 0 0.0
    CQ D 0 1e-15
    VD D 0 PWL(
    + 000.0e-9 0.0
    + 100.0e-9 0.0
    + 110.0e-9 5.0
    + 200.0e-9 5.0
    + 210.0e-9 0.0
    + 300.0e-9 0.0
    + 310.0e-9 5.0
    + 400.0e-9 5.0
    + )

    .include "jlpkg://GF180MCUPDK/model/design.ngspice"
    .LIB "jlpkg://GF180MCUPDK/model/sm141064.ngspice" typical

    .TRAN 1e-9 4.0e-7
    .END
    """
=#

#=
ckt = sp"""
* Sky130 test
X0 VDD VG 0 0 sky130_fd_pr__nfet_01v8 w=5 l=10 m=10
V1 VDD 0 1.8
Vg VG 0 sin(0.9 0.9 1e3)

.tran 1u 2m
.options acct noinit temp=25 reltol=1e-12

.lib jlpkg://Sky130PDK/sky130A/libs.tech/ngspice/sky130.lib.spice tt
"""
=#

#=
ckt = sp"""
* Subcircuit hierarchy test
.param area=10

.subckt hier1 1 2 area='w*l' w=3 l=3
.subckt hier2 1 2 area=4
.subckt hier3 1 2 area=8
r3 1 2 'area' ; = 30 in ngspice
.ends
x2 1 2 hier3 area='area'
.ends
x1 1 2 hier2 area='area'
.ends

x0 1 0 hier1 w=10 l=3
i1 1 0 1
"""
=#

ckt = sp"""
* Inverter Smoke test

Xneg VSS D Q VSS sg13_lv_nmos W=1.48u L=130n
Xpos VDD D Q VDD sg13_lv_pmos W=2.24u L=130n

VVDD VDD 0 1.8
VVSS VSS 0 0.0
CQ D 0 1e-15
VD D 0 PWL(
+ 000.0e-9 0.0
+ 100.0e-9 0.0
+ 110.0e-9 1.8
+ 200.0e-9 1.8
+ 210.0e-9 0.0
+ 300.0e-9 0.0
+ 310.0e-9 1.8
+ 400.0e-9 1.8
+ )

*.hdl "jlpkg://CMC/cmc_models/PSP103.8.2/psp103_nqs.va"
.hdl "/home/keno/.julia/packages/CMC/gDuGv/cmc_models/PSP103.8.2/psp103_nqs.va"
.LIB "jlpkg://IHP_SG13G2/models/cornerMOSlv.lib" mos_tt
.END
"""

display(ckt)
ckt()
