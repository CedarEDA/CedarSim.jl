using Test

using SpectreNetlistParser: SPICENetlistParser
using .SPICENetlistParser.SPICENetlistTokenize.Tokens
using .SPICENetlistParser.SPICENetlistTokenize: tokenize, Tokens, kind, next_token
using Lexers: is_triv

using DeepDiffs

token_test =
[
".TITLE THIS IS A TILE WITH ¤%&/)/& stuff" => [DOT, TITLE, TITLE_LINE]
"Vv-_-{} A B 0" => [IDENTIFIER_VOLTAGE, IDENTIFIER, IDENTIFIER, INT_LIT]
"* MOSFET" => nothing
".GLOBAL VDD NET1" => [DOT, GLOBAL, IDENTIFIER, IDENTIFIER]
".MODEL BJT_modName NPN (BF=val)" => [DOT, MODEL, IDENTIFIER, IDENTIFIER, IDENTIFIER, EQ, VAL]
"Rname N1 N2 0.1 \$ comment" => [IDENTIFIER_RESISTOR, IDENTIFIER, IDENTIFIER, FLOAT]
"sky130.0" => [IDENTIFIER_SWITCH, DOT, INT_LIT]
".param freq = 1Meg" => [DOT, PARAMETERS, IDENTIFIER, EQ, FLOAT]
".parameter freq = 1Meg" => [DOT, PARAMETERS, IDENTIFIER, EQ, FLOAT]
"*comment" => []
"    *comment" => []
";comment" => []
"    ;comment" => []
"1    ;comment" => [INT_LIT]
"1*1" => [INT_LIT, STAR, INT_LIT]
".lib 'with spaces/sm141064.ngspice' nmos_6p0_t" => [DOT, LIB, STRING, IDENTIFIER]
".lib sm141064.ngspice nmos_6p0_t" => [DOT, LIB, IDENTIFIER, IDENTIFIER]
".include ./foo" => [DOT, INCLUDE, IDENTIFIER]
".include './foo.bar'" => [DOT, INCLUDE, STRING]
".param r_l='s*(r_length-2*r_dl)'" => [DOT, PARAMETERS, IDENTIFIER, EQ, PRIME, IDENTIFIER, STAR, LPAREN,
                                      IDENTIFIER, MINUS, INT_LIT, STAR, IDENTIFIER, RPAREN, PRIME]
"Q1 Net-_Q1-C_ Net-_Q1-B_ 0 BC546B" => [IDENTIFIER_BIPOLAR_TRANSISTOR, IDENTIFIER, IDENTIFIER, INT_LIT, IDENTIFIER]
"X1 v+ v- r={a-b} f-o-o=1" => [IDENTIFIER_SUBCIRCUIT_CALL, IDENTIFIER, IDENTIFIER, IDENTIFIER, EQ,
                               LBRACE, IDENTIFIER, MINUS, IDENTIFIER, RBRACE, IDENTIFIER, EQ, INT_LIT]
".ic v( m_tn4:d )=  1.225e-08" => [DOT, IC, VAL, LPAREN, IDENTIFIER, COLON, IDENTIFIER, RPAREN, EQ, FLOAT]
"V1 vin 0 SIN (0, 1, 1k)" => [IDENTIFIER_VOLTAGE, IDENTIFIER, INT_LIT, SIN, INT_LIT, INT_LIT, FLOAT]
"R1 (a b) r=\"foo+bar\"" => [IDENTIFIER_RESISTOR, IDENTIFIER, IDENTIFIER, IDENTIFIER, EQ, PRIME, IDENTIFIER, PLUS, IDENTIFIER, PRIME]
".MEAS TRAN res1 FIND V(out) AT=5m" => [DOT, MEASURE, TRAN, IDENTIFIER, FIND, VAL, LPAREN, IDENTIFIER, RPAREN, AT, EQ, FLOAT]
".tran 1ns 60ns" => [DOT, TRAN, FLOAT, UNIT, FLOAT, UNIT]
"" => [ENDMARKER]
]

str = join((first(x) for x in token_test), '\n')
tokenized_kinds = filter(!is_triv, kind.(collect(tokenize(str, ERROR, next_token; case_sensitive=false))))
true_kinds = collect(Iterators.flatten([last(x) for x in token_test if last(x) !== nothing]))

if tokenized_kinds != true_kinds
    println(deepdiff(tokenized_kinds, true_kinds))
    error()
end
