using SpectreNetlistParser
using SpectreNetlistParser: SpectreNetlistCSTParser, SPICENetlistParser
using .SPICENetlistParser: SPICENetlistCSTParser
using .SpectreNetlistCSTParser:
    SpectreNetlistSource
using .SPICENetlistCSTParser:
    SPICENetlistSource
using VerilogAParser
const SNode = SpectreNetlistCSTParser.Node
const VANode = VerilogAParser.VerilogACSTParser.Node
const SC = SpectreNetlistCSTParser
const SP = SPICENetlistCSTParser


using CedarSim
include(joinpath(dirname(pathof(CedarSim)), "spc/cache.jl"))
include(joinpath(dirname(pathof(CedarSim)), "spc/interface.jl"))
include(joinpath(dirname(pathof(CedarSim)), "spc/sema.jl"))
include(joinpath(dirname(pathof(CedarSim)), "spc/codegen.jl"))
include(joinpath(dirname(pathof(CedarSim)), "spc/query.jl"))
include(joinpath(dirname(pathof(CedarSim)), "spc/generated.jl"))
