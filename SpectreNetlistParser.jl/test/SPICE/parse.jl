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

function test_recursive_all(root; fail_fast=false, verbose=false, expected_failures=String[])
    n = 1
    for (root, dirs, files) in walkdir(root)
        for file in files
            if contains(file, "_test.spice")
                verbose && @warn "Ignoring test $file"
                continue
            end
            file = joinpath(root, file)
            expected_to_fail = any(endswith(file, ef) for ef in expected_failures)
            str = read(file, String)
            if contains(str, ".control")
                verbose && "Ignoring $(repr(file)) has .control"
                continue
            end
            if endswith(file, ".spice")
                err = try
                    SPICENetlistParser.parsefile(file)
                    nothing
                catch e
                    e isa InterruptException && rethrow()
                    e
                end
                if err === nothing && !expected_to_fail
                    verbose && printstyled("Parsed [$n] $file\n"; color=:light_green)
                    @test true
                elseif err isa Exception && expected_to_fail
                    verbose && printstyled("Failed (expected) [$n] $file\n"; color=:light_green)
                    @test_broken false
                elseif err === nothing && expected_to_fail
                    printstyled("Parsed (unexpected) [$n] $file\n"; color=:light_red)
                    fail_fast && error("Unexpectedly parsed $file")
                    @test_broken true
                else # err isa Exception && !expected_to_fail
                    printstyled("Failed [$n] $file\n"; color=:light_red)
                    fail_fast && throw(err)
                    @test false
                end
                n+=1
            end
        end
    end
    return
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

root = abspath(sky130_root, "sky130A", "libs.ref", "sky130_fd_pr", "spice")
test_recursive(root; verbose=false)
test_recursive_all(
    sky130_root;
    fail_fast=false, verbose=false,
    expected_failures=["combined/parameters/invariant.spice"],
)

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

missing_implicit_title = """
v1 vcc 0 DC 5
r1 vcc n1 1k
l1 n1 n2 1m
c1 n2 0 1u
"""
ast = SPICENetlistParser.SPICENetlistCSTParser.parse(missing_implicit_title)
@test ast.expr.form.stmts[1].form isa SPICENetlistParser.SPICENetlistCSTParser.Title
@test ast.expr.form.stmts[2].form isa SPICENetlistParser.SPICENetlistCSTParser.Resistor

# A blank newline should count as an empty title
empty_implicit_title = """

v1 vcc 0 DC 5
r1 vcc n1 1k
l1 n1 n2 1m
c1 n2 0 1u
"""
ast = SPICENetlistParser.SPICENetlistCSTParser.parse(empty_implicit_title)
@test ast.expr.form.stmts[1].form isa SPICENetlistParser.SPICENetlistCSTParser.Title
@test ast.expr.form.stmts[2].form isa SPICENetlistParser.SPICENetlistCSTParser.Voltage

# Leading whitespace should be accepted & ignored if no implicit title is expected
no_implicit_title = """

v1 vcc 0 DC 5
r1 vcc n1 1k
l1 n1 n2 1m
c1 n2 0 1u
"""
ast = SPICENetlistParser.SPICENetlistCSTParser.parse(no_implicit_title; implicit_title=false)
@test ast.expr.form.stmts[1].form isa SPICENetlistParser.SPICENetlistCSTParser.Voltage

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

# Test if/else/endif
ifleseif = """
* If Test
.param x = 3
.if (x == 2)
.elseif (x == 3)
.elseif (x == 4)
.else
.endif
"""
ast = SPICENetlistParser.SPICENetlistCSTParser.parse(ifleseif)

let conditional_after_binop = """
* Conditional after Binop
.param x = 1
.param y = +(x<5 ? 1e5 : 1e-5)
"""
    ast = SPICENetlistParser.SPICENetlistCSTParser.parse(conditional_after_binop)
    @test ast.stmts[3].params[1].val.operand.inner.form isa SPICENetlistParser.SPICENetlistCSTParser.TernaryExpr
end
