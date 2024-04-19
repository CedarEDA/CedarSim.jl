#=
This code has been extracted from the DataStructures.jl package

Copyright (c) 2013 Dahua Lin

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
=#

module Tries

export Trie, subtrie

mutable struct Trie{K,V}
    value::V
    children::Dict{K,Trie{K,V}}
    is_key::Bool

    function Trie{K,V}() where {K,V}
        self = new{K,V}()
        self.children = Dict{K,Trie{K,V}}()
        self.is_key = false
        return self
    end

    function Trie{K,V}(ks, vs) where {K,V}
        return Trie{K,V}(zip(ks, vs))
    end

    function Trie{K,V}(kv) where {K,V}
        t = Trie{K,V}()
        for (k,v) in kv
            t[k] = v
        end
        return t
    end
end

Trie() = Trie{Any,Any}()
Trie(ks::AbstractVector{K}, vs::AbstractVector{V}) where {K,V} = Trie{eltype(K),V}(ks, vs)
Trie(kv::AbstractVector{Tuple{K,V}}) where {K,V} = Trie{eltype(K),V}(kv)
Trie(kv::AbstractDict{K,V}) where {K,V} = Trie{eltype(K),V}(kv)
Trie(ks::AbstractVector{K}) where {K} = Trie{eltype(K),Nothing}(ks, similar(ks, Nothing))

function Base.setindex!(t::Trie{K,V}, val, key) where {K,V}
    value = convert(V, val) # we don't want to iterate before finding out it fails
    node = t
    for char in key
        if !haskey(node.children, char)
            node.children[char] = Trie{K,V}()
        end
        node = node.children[char]
    end
    node.is_key = true
    node.value = value
end

function Base.getindex(t::Trie, key)
    node = subtrie(t, key)
    if node !== nothing && node.is_key
        return node.value
    end
    throw(KeyError("key not found: $key"))
end

function subtrie(t::Trie, prefix)
    node = t
    for char in prefix
        if !haskey(node.children, char)
            return nothing
        else
            node = node.children[char]
        end
    end
    return node
end

end
