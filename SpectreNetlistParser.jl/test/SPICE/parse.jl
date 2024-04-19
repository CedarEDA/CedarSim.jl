using SpectreNetlistParser: SPICENetlistParser

using .SPICENetlistParser
using .SPICENetlistParser.SPICENetlistCSTParser: EXPR, reducedcontent, Node
using Test
using AbstractTrees

var = """
* Implicit title
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

function check_roundtrip(var; broken=false)
    try
        s1 = SPICENetlistParser.parse(var)
        s1_str = reducedcontent(s1)
        s2 = SPICENetlistParser.parse(s1_str)
        s2_str = reducedcontent(s2)
        @test s1_str == s2_str
        return true # pass
    catch err
        if broken
            @test_broken false
        else
            @test false
        end
    end
    return false # fail
end

function check_roundtrip_file(file; broken=false)
    try
        s1 = SPICENetlistParser.parsefile(file)
        s1_str = reducedcontent(s1)
        s2 = SPICENetlistParser.parse(s1_str)
        s2_str = reducedcontent(s2)
        @test s1_str == s2_str
        return true # pass
    catch err
        if broken
            @test_broken false
        else
            @test false
        end
    end
    return false # fail
end

check_roundtrip(var)

failures = [
]

function test_examples(; verbose=false)
    for (root, dirs, files) in walkdir(joinpath(@__DIR__, "examples"))
        for file in files
            path = joinpath(root, file)
            broken = file in failures
            verbose && printstyled("SPICE parsing $(repr(file))"; color=:light_cyan)
            pass = check_roundtrip_file(path; broken)
            status = if broken && !pass
                "[not yet supported 😬]"
            elseif broken && pass
                "[working but marked as broken 😬]"
            elseif !broken && pass
                "✓"
            else
                "x"
            end
            verbose && printstyled(" $status\n"; color=:light_green)
        end
    end
    return
end

test_examples(verbose=true)

function pushfile!(new_frontier, all_files_parsed, file, node)
    for leaf in AbstractTrees.Leaves(node)
        if leaf isa Node{SPICENetlistParser.SPICENetlistCSTParser.StringLiteral}
            f = String(leaf)[2:end-1]
            path = abspath(joinpath(dirname(file), f))
            if !(path in all_files_parsed)
                push!(new_frontier, path) # Skip quotes in string
            end
        end
    end
end

function test_recursive_all(root; fail_fast=false, verbose=false)
    n = 1
    n_success = 0
    n_fail = 0
    for (root, dirs, files) in walkdir(root)
        for file in files
            if contains(file, "_test.spice")
                verbose && @warn "Ignoring test $file"
                continue
            end
            file = joinpath(root, file)
            str = read(file, String)
            if contains(str, ".control")
                verbose && "Ignoring $(repr(file)) has .control"
                continue
            end
            if endswith(file, ".spice")
                try
                    SPICENetlistParser.parsefile(file)
                    verbose && printstyled("Parsed [$n] $file\n"; color=:light_green)
                    n_success += 1
                catch e
                    e isa InterruptException && rethrow()
                    fail_fast && rethrow()
                    verbose && printstyled("Failed [$n] $file\n"; color=:light_red)
                    n_fail += 1
                end
                n+=1
            end
        end
    end
    verbose && print("Success: $n_success, Fail: $n_fail")
    return n_fail
end

function test_recursive(root; verbose=false)
    n = 1
    root = abspath(root)
    all_files_parsed = Set{String}()
    frontier = Set{String}()

    push!(frontier, root)

    while !isempty(frontier)
        new_frontier = Set{String}()

        for file in frontier
            verbose && @info "Parsing [$n] $file"
            if !endswith(file, ".spice")
                verbose && @warn "Ignoring $(repr(file)) not spice ending"
                continue
            end
            isfile(file) || error("did not find file $file")
            s = SPICENetlistParser.parsefile(file)
            push!(all_files_parsed, file)
            n += 1

            for node in children(s)
                if node isa Node{SPICENetlistParser.SPICENetlistCSTParser.IncludeStatement}
                    pushfile!(new_frontier, all_files_parsed, file, node)
                elseif node isa Node{SPICENetlistParser.SPICENetlistCSTParser.LibStatement}
                    for node2 in children(node)
                        if node2 isa Node{SPICENetlistParser.SPICENetlistCSTParser.IncludeStatement}
                            pushfile!(new_frontier, all_files_parsed, file, node2)
                        end
                    end
                end
            end
        end
        copy!(frontier, new_frontier)
    end
end

sky130_root = abspath(Base.find_package("Sky130PDK"), "..", "..")

root = abspath(sky130_root, "libraries", "sky130_fd_pr", "models", "sky130.lib.spice")
test_recursive(root; verbose=false)
n_fail = test_recursive_all(sky130_root; fail_fast=false, verbose=false)
@test n_fail == 0

function check_roundtrip(var; offset=0)
    s1 = SPICENetlistParser.SPICENetlistCSTParser.parse(var; offset)
    s1_str = reducedcontent(s1)
    s2 = SPICENetlistParser.SPICENetlistCSTParser.parse(s1_str)
    s2_str = reducedcontent(s2)
    @test s1_str == s2_str
end

with_offset = """
This is
some initial
garbage
* Implicit title
.DC Vds 0 5 0.5 Vgs 0 5 1
"""
check_roundtrip(with_offset; offset=29)


return_lang = """
* Implicit title
.DC Vds 0 5 0.5 Vgs 0 5 1
simulator lang=spectre
.GARBAGE
"""
SPICENetlistParser.SPICENetlistCSTParser.parse(return_lang; return_on_language_change=true)

weird_instance_name = """
* Implicit title
Vv-_-{} A B 0
"""
check_roundtrip(weird_instance_name)

title = """
* Implicit title
.title KiCad schematic
"""
# TODO: This introduces an extra space in round tripping
SPICENetlistParser.SPICENetlistCSTParser.parse(title)


# Test error
faulty = """

.FOO
"""
e = try SPICENetlistParser.SPICENetlistCSTParser.parse(faulty)
catch g
    g
end
str = sprint(io -> Base.showerror(io, e))
@test occursin("SPICEParser error at line 2:\n  .FOO\n", str)


using GF180MCUPDK
f = joinpath(pkgdir(GF180MCUPDK), "model", "sm141064.ngspice")
SPICENetlistParser.SPICENetlistCSTParser.parsefile(f)
