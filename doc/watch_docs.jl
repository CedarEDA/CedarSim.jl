# Allow loading of CedarSim, etc...
insert!(Base.LOAD_PATH, 1, dirname(@__DIR__))

using Weave
using FileWatching
while true
    path, info = FileWatching.watch_folder(@__DIR__)
    @show path, info
    _, ext = splitext(path)
    if ext == ".jmd"
        weave(path; doctype = "md2html", fig_path="fig")
    end
end 
