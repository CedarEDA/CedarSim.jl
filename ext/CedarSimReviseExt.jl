module CedarSimReviseExt
using CedarSim, Revise
using CedarSim: VAFile
using VerilogAParser

if isdefined(Revise, :is_same_file)
    function Revise.parse_source!(mod_exprs_sigs::Revise.ModuleExprsSigs, file::VAFile, mod::Module; kwargs...)
        va = VerilogAParser.parsefile(file.file)
        if va.ps.errored
            throw(LoadError(file.file, 0, VAParseError(va)))
        end
        ex = CedarSim.make_module(va)
        Revise.process_source!(mod_exprs_sigs, ex, file, mod; kwargs...)
    end
    Revise.is_same_file(a::VAFile, b::String) = a.file == b
end

end # module
