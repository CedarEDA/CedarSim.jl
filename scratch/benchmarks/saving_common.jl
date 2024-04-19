using Humanize
using Printf
using BenchmarkTools

script_name() = splitext(basename(PROGRAM_FILE))[1]

struct BenchmarkSaver
    name::String
    plain_fh::IO
    human_fh::IO
end
function BenchmarkSaver(name = script_name())
    BenchmarkSaver(
        name,
        open(joinpath(mkpath("output_stats"), "$name.tsv"), "w"),
        open(joinpath(mkpath("output_stats"), "$name.txt"), "w"),
    )
end
 
function save_value(saver, names, value, humanize_transform = string)
    # Flush immediately so we can use `tail -f file` to watch it live.
    print(saver.plain_fh, key(saver, names...))
    print(saver.plain_fh, "\t")
    print(saver.plain_fh, value)
    println(saver.plain_fh)
    flush(saver.plain_fh)

    print(saver.human_fh, key(saver, names...))
    print(saver.human_fh, ":\t")
    print(saver.human_fh, humanize_transform(value))
    println(saver.human_fh)
    flush(saver.human_fh)
end

function save_value(saver, names, trial::BenchmarkTools.Trial)
    best = minimum(trial)
    save_value(saver, (names..., "allocs"), best.allocs, num)
    save_value(saver, (names..., "memory"), best.memory, data)
    save_value(saver, (names..., "min time"), best.time, timedelta)
    save_value(saver, (names..., "mean time"), mean(trial).time, timedelta)
end

# Include saver name in the per row key so that when appended can still tell what problem was from 
key(saver::BenchmarkSaver, pernames...) = key(saver.name, pernames...)
key(pernames...) = join(pernames, " - ")

# post processing humanize_transforms
num(x::Integer) = Humanize.digitsep(x)
num(x::Real) = x<10_000 ? (@printf "%.4g" x) : num(round(Int, x))  # if over 10,000 we don't care about decimals but do want thousand seperators
data(x) = Humanize.datasize(x, style=:bin)
const timedelta = BenchmarkTools.prettytime  # Humanize doesn't support nanoseconds. https://github.com/IainNZ/Humanize.jl/issues/18