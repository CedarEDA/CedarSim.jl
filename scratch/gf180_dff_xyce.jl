using Xyce_jll
cd(joinpath(@__DIR__, ".."))
run(`$(Xyce()) test/DFF/DFF_cap_all_xyce.cir`)