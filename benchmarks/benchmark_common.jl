using BenchmarkTools
using DAECompiler: get_sys
using CedarSim
using BSIM4
using CedarSim.SpectreEnvironment
using SpectreNetlistParser: SPICENetlistParser
using TimerOutputs
using OrdinaryDiffEq
using GF180MCUPDK

include("saving_common.jl")

function compile_circuit(circuit, to, compute_jacobian, name)
    debug_config = (;
        ir_log = joinpath(@__DIR__, "output", "ir", name),
        store_ir_levels = true,
        store_ss_levels = true,
    )
    @timeit to "Compilation" begin
        @timeit to "CircuitIRODESystem(circuit)" begin
            sys = CircuitIRODESystem(circuit; debug_config)
        end

        @timeit to "DAEProblem(sys)" begin
            daeprob = DAEProblem(sys, nothing, nothing, (0.0, 7e-7); initializealg=CedarDCOp(;abstol=1e-15));
        end

        if compute_jacobian
            @timeit to "DAEProblem(sys; jac=true)" begin
                daeprob_jac = DAEProblem(sys, nothing, nothing, (0.0, 7e-7); initializealg=CedarDCOp(;abstol=1e-15), jac=true);
            end
        else
            daeprob_jac = nothing
        end

        @timeit to "ODEProblem(sys)" begin
            odeprob = ODEProblem(sys, nothing, (0.0, 7e-7); initializealg=CedarDCOp(;abstol=1e-15));
        end

        if compute_jacobian
            @timeit to "ODEProblem(sys; jac=true)" begin
                odeprob_jac = ODEProblem(sys, nothing, (0.0, 7e-7); initializealg=CedarDCOp(;abstol=1e-15), jac=true);
            end
        else
            odeprob_jac = nothing
        end
    end
    return daeprob, daeprob_jac, odeprob, odeprob_jac
end

function load_gf180(;to = TimerOutput(), compute_jacobian::Bool = true)
    repo_root = Base.pkgdir(CedarSim)
    dffdir = joinpath(repo_root, "test", "DFF")
    @timeit to "Parsing" begin
        @timeit to "include(BSIM4.bsim4_va)" begin
            if !isdefined(@__MODULE__, :bsim4)
                @eval const bsim4 = load_VA_model(BSIM4.bsim4_va)
            end
        end
        @timeit to "SPICENetlistParser.parsefile" begin
            sa1 = SPICENetlistParser.parsefile(joinpath(dffdir, "DFF_cap_all.cir"))
        end
        @timeit to "CedarSim.make_spectre_circuit" begin
            code = CedarSim.make_spectre_circuit(sa1, String[repo_root, dffdir]);
        end
        @timeit to "eval(code)" begin
            circuit = eval(code)
        end
    end
    return compile_circuit(circuit, to, compute_jacobian, "dff")
end

function load_inverter(;to = TimerOutput(), compute_jacobian::Bool = true)
    repo_root = Base.pkgdir(CedarSim)
    @timeit to "Parsing" begin
        @timeit to "include(BSIM4.bsim4_va)" begin
            if !isdefined(@__MODULE__, :bsim4)
                @eval const bsim4 = load_VA_model(BSIM4.bsim4_va)
            end
        end

        spice_code = """
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

        .LIB "jlpkg://GF180MCUPDK/sm141064.ngspice" typical

        .TRAN 1e-9 4.0e-7
        .END
        """
        @timeit to "SPICENetlistCSTParser.parse" begin
            sa1 = CedarSim.SpectreNetlistParser.SPICENetlistParser.SPICENetlistCSTParser.parse(spice_code)
        end
        @timeit to "CedarSim.make_spectre_circuit" begin
            code = CedarSim.make_spectre_circuit(sa1, String[repo_root]);
        end
        @timeit to "eval(code)" begin
            circuit = eval(code)
        end
    end
    return compile_circuit(circuit, to, compute_jacobian, "inverter")
end

"Returns the last IR level with the given prefix"
function get_ir_level(prob, prefix)
    ir_levels = getfield(get_sys(prob), :debug_config).ir_levels
    matching_levels = [l for l in ir_levels if startswith(l.name, prefix)]
    if isempty(matching_levels)
        throw(ArgumentError("Asked for an IR level prefix $(prefix) that does not exist!"))
    end
    return last(matching_levels).ir
end

function benchmark_prob(prob)
    saver = BenchmarkSaver()
    out = similar(prob.u0)
    f_bench = nothing
    jac_bench = nothing
    tgrad_bench = nothing
    name = isa(prob, DAEProblem) ? "DAEProblem" : "ODEProblem"
    if isa(prob, DAEProblem)
        f_bench = @benchmark $(prob).f.f($(out), $(prob).du0, $(prob).u0, $(prob).p, 0.0)
    elseif isa(prob, ODEProblem)
        f_bench = @benchmark $(prob).f.f($(out), $(prob).u0, $(prob).p, 0.0)
    end

    println("--- $(name) RHS")
    ir_statements = length(get_ir_level(prob, "$(name).dae_finish!").stmts)
    num_states = length(out)
    @info("$(name) benchmark:", num_states, ir_statements)
    display(f_bench)
    save_value(saver, (name, "RHS", "ir_statements"), ir_statements, num)
    save_value(saver, (name, "RHS", "num_states"), num_states, num)
    save_value(saver, (name, "RHS"), f_bench)

    if prob.f.jac !== nothing
        J = similar(prob.u0, (length(prob.u0), length(prob.u0)))
        if isa(prob, DAEProblem)
            jac_bench = @benchmark $(prob).f.jac($(J), $(prob).du0, $(prob).u0, $(prob).p, 1.0, 0.0)
        elseif isa(prob, ODEProblem)
            jac_bench = @benchmark $(prob).f.jac($(J), $(prob).u0, $(prob).p, 0.0)
        end

        println("--- $(name) Jacobian")
        ir_statements = length(get_ir_level(prob, "$(name).construct_jacobian").stmts)
        @info("$(name) Jacobian:", size = size(J), ir_statements)
        display(jac_bench)
        save_value(saver, (name, "jac", "ir_statements"), ir_statements, num)
        save_value(saver, (name, "jac"), jac_bench)
    end

    if prob.f.tgrad !== nothing
        if isa(prob, DAEProblem)
            tgrad_bench = @benchmark $(prob).f.tgrad($(out), $(prob).du0, $(prob).u0, $(prob).p, 0.0)
        elseif isa(prob, ODEProblem)
            tgrad_bench = @benchmark $(prob).f.tgrad($(out), $(prob).u0, $(prob).p, 0.0)
        end
        println("--- $(name) tgrad")
        ir_statements=length(get_ir_level(prob, "$(name).construct_tgrad").stmts)
        @info("$(name) tgrad:", num_states = length(out), ir_statements)
        display(tgrad_bench)
        save_value(saver, (name, "tgrad", "ir_statements"), ir_statements, num)
        save_value(saver, (name, "tgrad"), tgrad_bench)
    end
    return f_bench, jac_bench, tgrad_bench
end
