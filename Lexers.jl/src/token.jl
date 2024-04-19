struct Token{K}
    kind::K
    startbyte::Int # The byte where the token start in the buffer
    endbyte::Int # The byte where the token ended in the buffer
end

Token(k::K) where {K} = Token(k, 0, -1)

# Disallow comparisons between tokens and kinds. This is an easy error to make
# and we want it to complain loudly.
Base.:(==)(a::Token, b) = error("This doesn't do what you want")
Base.:(==)(a, b::Token) = error("This doesn't do what you want")

kind(t::Token) = t.kind
startbyte(t::Token) = t.startbyte
endbyte(t::Token) = t.endbyte

function untokenize(t::Token, str::String)
    String(codeunits(str)[1 .+ (t.startbyte:t.endbyte)])
end

function Base.show(io::IO, t::Token)
    start_b, end_b = startbyte(t), endbyte(t)
    print(io, rpad(string(start_b, "-", end_b), 17, " "))
    print(io, rpad(kind(t), 15, " "))
end
