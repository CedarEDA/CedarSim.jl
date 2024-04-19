using Test

using SpectreNetlistParser
using SpectreNetlistParser.SpectreNetlistTokenize.Tokens
using SpectreNetlistParser.SpectreNetlistTokenize: tokenize, Tokens, kind, next_token
using Lexers: is_triv
using DeepDiffs

token_test =
[
"// foo" => nothing
"011" => [INT_LIT]
"1.0" => [FLOAT]
"vdd!" => [IDENTIFIER]
"tan" => [TAN]
"tanh" => [TANH]
"march" => [MARCH]
"int" => [INT]
"2pf"=> [FLOAT, UNIT]
"6.3ns" => [FLOAT, UNIT]
"6_Ohms" => [FLOAT, UNIT]
"0.3MHz" => [FLOAT, UNIT]
"MHz" => [IDENTIFIER]
"a = 1 \\\nb=2" => [IDENTIFIER, EQ, INT_LIT, ESCD_NEWLINE, IDENTIFIER, EQ, INT_LIT]
"name info info=foo" => [IDENTIFIER, INFO, INFO, EQ, IDENTIFIER]
"tran tran tran=tran" => [TRAN, TRAN, TRAN, EQ, TRAN]
"save save=foo" => [SAVE, SAVE, EQ, IDENTIFIER]
"* comment" => nothing
"" => [ENDMARKER]
]

str = join((first(x) for x in token_test), '\n')
tokenized_kinds = filter(!is_triv, kind.(collect(tokenize(str, ERROR, next_token))))
true_kinds = collect(Iterators.flatten([last(x) for x in token_test if last(x) !== nothing]))

if tokenized_kinds != true_kinds
    println(deepdiff(tokenized_kinds, true_kinds))
    error()
end
