# Verilog-A Parser Documentation

## Overview

This documentation provides guidance on using the Verilog-A Parser. This parser is designed to handle the analog portion of the Verilog-AMS language, which is commonly used for creating analog, and mixed-signal models. For full documentation of the Verilog-A language see [verilogams.com](https://verilogams.com).  The parser facilitates the extraction of the Abstract Syntax Tree (AST) from Verilog-A source files, which can be further utilized for analysis, simulation, or conversion purposes.

## Installation

Before using the Verilog-A Parser, ensure that the `CedarSim` package, which includes the `VerilogAParser`, is installed in your Julia environment. You can install `CedarSim` using Julia's package manager.

```julia
using Pkg
Pkg.add("CedarSim")
```

## Usage

To parse a Verilog-A file and obtain the AST, follow these steps:

### Importing the Module

First, you need to import the `CedarSim` package, specifically the `VerilogAParser` module.

```julia
using CedarSim: VerilogAParser
```

### Parsing a File

To parse a Verilog-A file, use the `parsefile` function provided by the `VerilogAParser`. You need to specify the path to your Verilog-A file as an argument to this function.

```julia
ast = VerilogAParser.parsefile("path/to/your/verilogA_file.va")
```

This function reads the specified Verilog-A file and parses it to construct the AST.

### Example

Here's a simple example of how to use the Verilog-A Parser to parse a file and print a summary of the AST:

```julia
# Import the necessary module
using CedarSim: VerilogAParser

# Specify the path to your Verilog-A file
filename = "example.va"

# Parse the file to obtain the AST
ast = VerilogAParser.parsefile(filename)

# Print the AST to the console
println(ast)
```

This script will output the structure of the AST, which provides insights into the parsed elements of the Verilog-A file.

## Conclusion

The Verilog-A Parser in Julia provides a powerful tool for handling the parsing of analog Verilog-AMS models. By converting Verilog-A code into an AST, users can perform a wide range of tasks from analysis to simulation, enhancing the flexibility and utility of Verilog-A model handling in Julia.

Please file an issue on our [public issue tracker](https://github.com/CedarEDA/PublicIssues) for Verilog-A features that are important to you, but are not yet implemented.
