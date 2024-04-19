.MEAS TRAN res1 FIND V(out) AT=5m
.MEAS TRAN res2 FIND V(out)*I(Vout) WHEN V(x)=3*V(y)
.MEAS TRAN res3 FIND V(out) WHEN V(x)=3*V(y) cross=3
.MEAS TRAN res4 FIND V(out) WHEN V(x)=3*V(y) rise=last
.MEAS TRAN res5 FIND V(out) WHEN V(x)=3*V(y) cross=3 TD=1m
.MEAS TRAN res6 PARAM 3*res1/res2
.MEAS TRAN res6 WHEN V(x)=3*V(y)
 
.MEAS TRAN res7 AVG V(NS01)
+ TRIG V(NS05) VAL=1.5 TD=1.1u FALL=1
+ TARG V(NS03) VAL=1.5 TD=1.1u FALL=1

.MEAS AC rel8 when V(out)=1/sqrt(2)

.MEAS AC tmp max mag(V(out))

.MEAS AC BW trig mag(V(out))=tmp/sqrt(2) rise=1
+ targ mag(V(out))=tmp/sqrt(2) fall=last

.MEAS NOISE out_totn INTEG V(onoise)
.MEAS NOISE in_totn INTEG V(inoise)

.measure tran PARAM_pvtsc  param=par('pvtsc')