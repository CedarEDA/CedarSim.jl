* One-Bit Comparator (tran)

.option reltol=1e-3 post

.include modelcard.nmos
.include modelcard.pmos

Vdd Vdd 0 1.8
Va A 0 pulse 0 1.8 10ns .1ns .1ns 15ns 30ns
Vb B 0 0

M1 Anot A Vdd Vdd P1 W=3.6u L=0.2u
M2 Anot A 0 0 N1 W=1.8u L=0.2u
M3 Bnot B Vdd Vdd P1 W=3.6u L=0.2u
M4 Bnot B 0 0 N1 W=1.8u L=0.2u
M5 AorBnot 0 Vdd Vdd P1 W=1.8u L=3.6u
M6 AorBnot B 1 0 N1 W=1.8u L=0.2u
M7 1 Anot 0 0 N1 W=1.8u L=0.2u
M8 Lnot 0 Vdd Vdd P1 W=1.8u L=3.6u
M9 Lnot Bnot 2 0 N1 W=1.8u L=0.2u
M10 2 A 0 0 N1 W=1.8u L=0.2u
M11 Qnot 0 Vdd Vdd P1 W=3.6u L=3.6u
M12 Qnot AorBnot 3 0 N1 W=1.8u L=0.2u
M13 3 Lnot 0 0 N1 W=1.8u L=0.2u
MQLO 8 Qnot Vdd Vdd P1 W=3.6u L=0.2u
MQL1 8 Qnot 0 0 N1 W=1.8u L=0.2u
MLTO 9 Lnot Vdd Vdd P1 W=3.6u L=0.2u
MLT1 9 Lnot 0 0 N1 W=1.8u L=0.2u
CQ Qnot 0 30f
CL Lnot 0 10f

.tran 1n 60n
.print tran a b v(9) v(8)

.end

