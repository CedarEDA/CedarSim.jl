# Introduction to the `SpectreNetlistParser`

When working with netlists or creating EDA workflows, engineers and designers frequently
encounter the need to manipulate or analyze circuit designs expressed in netlist files.
Spectre and/or SPICE format netlists define the components and connections of a circuit in a text format.
The `SpectreNetlistParser` is a specialized tool designed to parse these netlist files
into an Abstract Syntax Tree (AST), transforming raw netlist data into a structured and
easily navigable format.

### Why Use `SpectreNetlistParser`?

The `SpectreNetlistParser` enhances the efficiency and accuracy of circuit design and simulation processes in several key ways:

- **Ease of Use**: It automates the conversion of text-based netlist files into ASTs, which accurately represent the hierarchical structure of a circuit, including its components and connectivity.
- **Accuracy**: The parsed netlist accurately maintains the original netlist structure including all comments and whitespace in a structured format.
- **Integration and Flexibility**: The parser seamlessly integrates with Julia, enabling it to be used in a wide range of applications.

By using `SpectreNetlistParser`, users can avoid the error-prone process of manually parsing complex netlist syntax
and can directly interact with the structure of the netlist programmatically.
This leads to more robust design practices and can significantly speed up the development cycle in electronic design projects.

## Getting Started with SpectreNetlistParser

To utilize `SpectreNetlistParser` in your projects, ensure Julia is installed on your system along with CedarSim. This tool is designed to fit smoothly into any Julia-based project or script where handling Spectre netlist files is required.

### Basic Usage

The following example illustrates the basic usage of the `SpectreNetlistParser`:

To read in a Spectre format netlist:

```julia
using CedarSim: SpectreNetlistParser
# Parse the netlist file
ast = SpectreNetlistParser.parsefile("inverter.scs")

# Now `ast` holds the abstract syntax tree of the netlist
```

or in SPICE format:


```julia
using CedarSim.SpectreNetlistParser: SPICENetlistParser
# Parse the netlist file
ast = SPICENetlistParser.parsefile("inverter.spice")

# Now `ast` holds the abstract syntax tree of the netlist
```

In this example, `parsefile` is a function that reads a netlist file named `inverter.scs` (or `inverter.spice`)
and parses it into an abstract syntax tree. The `ast` object can then be used to explore, analyze
the circuit described in the netlist.

## SPICE Syntax Support

[SPICE](https://en.wikipedia.org/wiki/SPICE) defines the connections between circuit elements, hence the files being referred to as "netlists".
CedarEDA supports the following syntax elements in SPICE, please file an issue on our [public issue tracker](https://github.com/CedarEDA/PublicIssues) for SPICE features that are important to you, but are not yet implemented.

### SPICE Elements

Type            | Element  | Supported Arguments
----------------|----------|----------------------------
Voltage Source  | `V`      | `DC`, `SIN`, `PULSE`, `PWL`
Current Source  | `I`      | `DC`, `SIN`, `PULSE`, `PWL`
Ideal Resistor  | `R`      |
Ideal Capacitor | `C`      |
Ideal Inductor  | `L`      |
MOSFET          | `M`      |
Subcircuit Call | `X`      | Instance parameter values


### SPICE Commands

Supported Commands | Example
-------------------|----------
`.ac`              | `.AC DEC 10 fmin fmax`
`.dc`              | `.NOISE V(vout) v0 0`
`.end`             | `.end`
`.global`          | `.global 0`
`.include`         | `.include "file.sp"`
`.lib`             | `.lib "models.lib" tt`
`.model`           | `.model nmos nmos ...`
`.op`              | `.op`
`.option`          | `.option RELTOL=1e-5`
`.options`         | `.options RELTOL=1e-5`
`.param`           | `.param c1 = 1p`
`.parameter`       | `.parameter rl = 10k`
`.parameters`      | `.parameters a=10 b=12`
`.print`           | `.print vin vout`
`.subckt`, `.ends` | `.subckt foo n1 n2 n3 x=1 y=10 ...`
`.tran`            | `.tran 1p 1u`


### SPICE Numbers
Numbers in SPICE are case insensitive and support the following scaling suffixes:

Suffix   | Scale
-------  |------
`T`      | 10^12
`G`      | 10^9
`MEG`    | 10^6
`k`      | 10^3
`m`      | 10^-3
`mil`    | 25.4u
`u`      | 10^-6
`n`      | 10^-9
`p`      | 10^-12
`f`      | 10^-15
`a`      | 10^-18

### SPICE Equations

The following mathematical operators are supported:

Operator | Meaning
---------|--------
`+`      | Addition
`-`      | Subtraction
`*`      | Multiplication
`/`      | Division
`**`     | Exponent

The following mathematical functions are supported:

Function     | Description
-------------|-----------------------------------
`min(x, y)`  | Minimum of `x` and `y`
`max(x, y)`  | Maximum of `x` and `y`
`abs(x)`     | Absolute value of `x`
`log(x)`     | Logarithm of `x` to base ℯ
`log10(x)`   | Logarithm of `x` to base 10
`sgn(x)`     | Sign of `x`: `x>0 = 1`, `x<0 = 1`, `x==0 = 0`
`sign(x, y)` | Magnitude of `x` with the sign of `y`: `sgn(y)*abs(x)`
`exp(x)`     | `ℯ^x`
`pow(x,y)`   | `x` to the power of `y`
`pwr(x,y)`   | Signed power: `sgn(x)*(abs(x)^y)`
`sqrt(x)`    | Square root of `x`
`sinh(x)`    | Hyperbolic sine of `x`
`cosh(x)`    | Hyperbolic cosine of `x`
`tanh(x)`    | Hyperbolic tangent of `x`
`sin(x)`     | Sine of `x` (in radians)
`cos(x)`     | Cosine of `x` (in radians)
`tan(x)`     | Tangent of `x` (in radians)
`atan(x)`    | Inverse tangent of `x`
`arctan(x)`  | Inverse tangent of `x`
`asinh(x)`   | Inverse hyperbolic sine of `x`
`acosh(x)`   | Inverse hyperbolic cosine of `x`
`atanh(x)`   | Inverse hyperbolic tangent of `x`
`int(x)`     | Integer portion of `x`
`nint(x)`    | Round to nearest integer to `x`
`floor(x)`   | Integer value less than or equal to `x`
`ceil(x)`    | Integer value greater than or equal to `x`



## Supported Spectre Syntax

Spectre refers to the netlist language used by the [Spectre Circuit Simulator](https://en.wikipedia.org/wiki/Spectre_Circuit_Simulator) and is closely related to SPICE.
`SpectreNetlistParser` supports the following syntax elements in Spectre, please file an issue on our [public issue tracker](https://github.com/CedarEDA/PublicIssues) for Spectre features that are important to you, but are not yet implemented.

### Spectre Elements

Type            | Spectre Element
----------------|-----------------
Voltage Source  | `vsource`
Current Source  | `isource`
Ideal Resistor  | `resistor`
Ideal Capacitor | `capactor`
Ideal Inductor  | `inductor`
Behavioral Source | `bsource`

### Spectre Numbers
Numbers in Spectre support the following scaling suffixes:

Suffix | Scale
-------|------
`T`    | 10^12
`G`    | 10^9
`M`    | 10^6
`K`    | 10^3
`k`    | 10^3
`_`    | 1
`%`    | 10^-2
`c`    | 10^-2
`m`    | 10^-3
`u`    | 10^-6
`n`    | 10^-9
`p`    | 10^-12
`f`    | 10^-15
`a`    | 10^-18

### Spectre Equations

The following mathematical operators are supported:

Operator | Meaning
---------|--------
`+`      | Addition
`-`      | Subtraction
`*`      | Multiplication
`/`      | Division
`^`      | Exponentiation
`!`      | Boolean not
`==`      | Boolean equal
`!=`      | Boolean not equal
`>`      | Greater than
`<`      | Less than
`<=`      | Less than or equal
`>=`      | Greater than or equal

The following mathematical functions are supported

Function     | Description
-------------|-------------
`max(x, y)`  | Maximum of `x` and `y`
`min(x, y)`  | Minimum of `x` and `y`
`abs(x)`     | Absolute value of `x`
`ln(x)`      | Logarithm of `x` to base ℯ
`log(x)`     | Logarithm of `x` to base ℯ
`log10(x)`   | Logarithm of `x` to base 10
`exp(x)`     | `ℯ^x`
`pow(x,y)`   | `x` to the power of `y`
`sqrt(x)`    | Square root of `x`
`sinh(x)`    | Hyperbolic sine of `x`
`cosh(x)`    | Hyperbolic cosine of `x`
`tanh(x)`    | Hyperbolic tangent of `x`
`sin(x)`     | Sine of `x` (in radians)
`cos(x)`     | Cosine of `x` (in radians)
`tan(x)`     | Tangent of `x` (in radians)
`atan(x)`    | Inverse tangent of `x`
`arctan(x)`  | Inverse tangent of `x`
`asinh(x)`   | Inverse hyperbolic sine of `x`
`acosh(x)`   | Inverse hyperbolic cosine of `x`
`atanh(x)`   | Inverse hyperbolic tangent of `x`
`int(x)`     | Integer value less than or equal to `x`
`floor(x)`   | Integer value less than or equal to `x`
`ceil(x)`    | Integer value greater than or equal to `x`

### Spectre Commands

Supported Commands | Example
-------------------|--------------
`ahdl_include`     | `ahdl_include "opamp.va"`
`dc`               | `dcOp dc write="spectre.dc"`
`global`           | `global 0`
`include`          | `include "inverter.scs"`
`info`             | `finalTimeOP info what=oppoint where=rawfile`
`inline subckt`    | `inline subckt pfet (d g s x) ...`
`model`            | `model mnfet bsimcmg type=n ...`
`noise`            | `noise noise start=10Hz stop=10M dec=1k`
`options`          | `saveOptions options save=allpub`
`parameters`       | `parameters rl=10k freq=1M`
`pnoise`           | `pnoise pnoise start=10Hz stop=10M dec=1k`
`pss`              | `pss pss fund=100k maxacfreq=4M`
`sp`               | `sp sp ports=[PORT0] start=1G stop=9G lin=101`
`subckt`, `ends`   | `subckt dff A CLK OUT VCC ...`
`save`             | `save VOUT`
`tran`             | `tran tran stop=1u start=0 step=1p`

## Conclusion

The `SpectreNetlistParser` provides a powerful, efficient way to handle Spectre netlists, transforming them from simple text files into structured, programmatically manageable data. This capability is crucial for accelerating development cycles and enhancing the accuracy and flexibility of circuit design and analysis. Whether for basic modifications, in-depth analyses, or comprehensive reporting, `SpectreNetlistParser` is an essential tool in the electronic design automation toolkit.

