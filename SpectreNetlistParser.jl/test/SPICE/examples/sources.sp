* Source tests
Vdd vdd 0 DC=1
Vdd vdd 0 DC=1v
vdd vdd 0 DC=1volts
idd vdd 0 DC=1mv
Vdd vdd 0 DC=1mvolts
Vdd vdd 0 DC 1
vdd vdd 0 DC 1v
Vdd vdd 0 DC 1volts
Idd vdd 0 DC 1mv
Vdd vdd 0 DC 1mvolts
Vdd vdd 0 AC=1
Vdd vdd 0 AC=1 30
Vdd vdd 0 AC=1, 90.0
Vdd vdd 0 AC=1, 90.0 DC=1.8
Vdd vdd 0 AC 1
Vdd vdd 0 AC 1 30
Vdd vdd 0 AC 1, 90.0
Vdd vdd 0 AC 1, 90.0 DC 1.8
idd vdd 0 PWL(0 1 1p 0)
Vdd vdd 0 PWL(0, 1, 1p, 0)
Vdd vdd 0 PWL(0 1, 1p 0)
vdd vdd 0 PWL 0 1 1p 0
Vdd vdd 0 PWL 0, 1, 1p, 0
Vdd vdd 0 PWL 0 1, 1p 0
.param t=1p
Vdd vdd 0 PWL(0 1 t 0)
Vdd vdd 0 PWL(0 1 t 0)
Vdd vdd 0 PWL(0 1 '2*t+3p' 0)
Vdd vdd 0 PWL('0*t' 1 '2*t' 0)
Vdd vdd 0 PWL('0*t' 1volts '2*t' 0volts)
Vdd vdd 0 PWL 0 1 t 0
idd vdd 0 PWL 0 1 t 0
vdd vdd 0 PWL 0 1 '2*t+3p' 0
Vdd vdd 0 PWL '0*t' 1 '2*t' 0
Vdd vdd 0 PWL '0*t' 1volts '2*t' 0volts

vdd vdd 0
+ PWL(
+      '0*t' 1volts
+      '2*t' 0volts
+ )

Idd vdd 0
+ PWL
+      '0*t' 1volts
+      '2*t' 0volts
+

Idd vdd 0
+ PWL
* This is a comment in the middle of a line wrap (ignore)
+      '0*t' 1volts
* This is a comment in the middle of a line wrap (ignore)
+      '2*t' 0volts
VCLKN           CLKN 0 PWL(
+ 000000.0e-12 5.0
+ 300000.0e-12 5.0
+ 301020.0e-12 0.0
+ 400000.0e-12 0.0
+ )

Ipulse 0 vdd PULSE(-1 2)
Vpulse 0 vdd pulse(-1 2 3p)
Vpulse 0 vdd PULSE(-1 2 3p 4p)
Ipulse 0 vdd PULse(-1 2 3p 4p 5p)
Vpulse 0 vdd pULSE(-1 2 3p 4p 5p 6p)
Vpulse 0 vdd Pu(-1 2)
Ipulse 0 vdd PU(-1 2 3p)
Vpulse 0 vdd pU(-1 2 3p 4p)
Ipulse 0 vdd PU(-1 2 3p 4p 5p)
Vpulse 0 vdd pu(-1 2 3p 4p 5p 6p)

Ipulse 0 vdd PULSE -1 2
Vpulse 0 vdd PULSE -1 2 3p
Vpulse 0 vdd PULSE -1 2 3p 4p
Ipulse 0 vdd PULSE -1 2 3p 4p 5p
Vpulse 0 vdd PULSE -1 2 3p 4p 5p 6p
Ipulse 0 vdd PU -1 2
Vpulse 0 vdd PU -1 2 3p
Ipulse 0 vdd PU -1 2 3p 4p
Vpulse 0 vdd PU -1 2 3p 4p 5p
Ipulse 0 vdd PU -1 2 3p 4p 5p 6p

Isin n1 n2 SIN(vo va)
Vsin n1 n2 SIN(vo va freq)
Isin n1 n2 SIN(vo va freq td)
Vsin n1 n2 SIN(vo va freq td q)
Isin n1 n2 SIN(vo va freq td q j)

Vsin n1 n2 SIN vo va
Isin n1 n2 sIN vo va freq
Vsin n1 n2 SiN vo va freq td
Isin n1 n2 SIn vo va freq td q
Vsin n1 n2 sin vo va freq td q j

vin in 0 sin(0.9600 0.01 100k) ac 1
vin in 0 sin(0.9600 0.01 100k) dc 1