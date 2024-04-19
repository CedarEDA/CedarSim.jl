module Lexers

export Lexer, Token, emit, emit, accept, accept_batch, tokenize, untokenize, kind, peekchar, dpeekchar, readchar

function is_triv end
is_newline(x) = false

include("token.jl")
include("lexer.jl")

end # module
