using VerilogAParser
using Test

do_regenerate = length(ARGS) >= 1 && (ARGS[1] == "--regenerate")

dir = joinpath(@__DIR__, "errors")
for test in readdir(dir)
    endswith(test, ".va") || continue
    do_regenerate && println("Regenerating $test")

    test = joinpath(dir, test)

    expected_output = string(test[1:end-3], ".out")

    local va = VerilogAParser.parsefile(test)
    buf = IOBuffer()
    out = IOContext(buf, :color=>true, :displaysize => (80, 240))
    VerilogAParser.VerilogACSTParser.visit_errors(va; io=out)

    out = String(take!(buf))

    if do_regenerate
        open(expected_output, "w") do f
            write(f, out)
        end
    else
        out_expected = read(expected_output, String)
        @test out == out_expected
    end
end
