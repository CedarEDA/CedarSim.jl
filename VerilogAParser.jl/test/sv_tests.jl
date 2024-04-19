numbers_blacklist = [
    "number_test_2.sv", # unbased_unsized_literal
    "number_test_3.sv", # unbased_unsized_literal
]

# For now, just make sure these all parse
dir = joinpath(@__DIR__, "sv-tests", "tests", "generic", "number")
@testset "Number tests" begin
    for test in readdir(dir)
        test in numbers_blacklist && continue

        @testset "$test" begin
            @test isa(VerilogAParser.parsefile(joinpath(dir, test)),
                Node{VerilogSource})
        end
    end
end

preproc_blacklist = [
    "preproc_test_2.svh", # For `include only
]

dir = joinpath(@__DIR__, "sv-tests", "tests", "generic", "preproc")
for test in readdir(dir)
    test in preproc_blacklist && continue

    @test isa(VerilogAParser.parsefile(joinpath(dir, test)),
        Node{VerilogSource})
end
