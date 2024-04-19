
const reserved_words = Dict{String, Kind}(
    "above" => RESERVED,
    "absdelay" => RESERVED,
    "absdelta" => RESERVED,
    "abstol" => ABSTOL,
    "access" => ACCESS,
    "ac_stim" => RESERVED,
    "aliasparam" => ALIASPARAM,
    "always" => RESERVED,
    "analog" => ANALOG,
    "analysis" => RESERVED,
    "and" => RESERVED,
    "assert" => RESERVED,
    "assign" => RESERVED,
    "automatic" => RESERVED,
    "begin" => BEGIN,
    "branch" => BRANCH,
    "buf" => RESERVED,
    "bufif0" => RESERVED,
    "bufif1" => RESERVED,
    "case" => CASE,
    "casex" => CASEX,
    "casez" => CASEZ,
    "cell" => RESERVED,
    "cmos" => RESERVED,
    "config" => RESERVED,
    "connect" => RESERVED,
    "connectmodule" => CONNECTMODULE,
    "connectrules" => RESERVED,
    "continuous" => CONTINUOUS,
    "cross" => RESERVED,
    "ddt_nature" => DDT_NATURE,
    "deassign" => RESERVED,
    "default" => DEFAULT,
    "defparam" => RESERVED,
    "design" => RESERVED,
    "disable" => RESERVED,
    "discipline" => DISCIPLINE,
    "discrete" => DISCRETE,
    "domain" => DOMAIN,
    "driver_update" => RESERVED,
    "edge" => RESERVED,
    "else" => ELSE,
    "end" => END,
    "endcase" => ENDCASE,
    "endconfig" => ENDCONFIG,
    "endconnectrules" => ENDCONNECTRULES,
    "enddiscipline" => ENDDISCIPLINE,
    "endfunction" => ENDFUNCTION,
    "endgenerate" => RESERVED,
    "endmodule" => ENDMODULE,
    "endnature" => ENDNATURE,
    "endparamset" => ENDPARAMSET,
    "endprimitive" => ENDPRIMITIVE,
    "endspecify" => RESERVED,
    "endtable" => RESERVED,
    "endtask" => RESERVED,
    "event" => RESERVED,
    "exclude" => EXCLUDE,
    "final_step" => RESERVED,
    "flicker_noise" => FLICKER_NOISE,
    "flow" => FLOW,
    "for" => FOR,
    "force" => RESERVED,
    "forever" => RESERVED,
    "fork" => RESERVED,
    "from" => FROM,
    "function" => FUNCTION,
    "generate" => RESERVED,
    "genvar" => RESERVED,
    "ground" => RESERVED,
    "highz0" => RESERVED,
    "highz1" => RESERVED,
    "idt" => IDT,
    "idtmod" => IDTMOD,
    "idt_nature" => IDT_NATURE,
    "if" => IF,
    "ifnone" => RESERVED,
    "incdir" => RESERVED,
    "include" => RESERVED,
    "inf" => INF,
    "initial" => INITIAL,
    "initial_step" => RESERVED,
    "inout" => INOUT,
    "input" => INPUT,
    "instance" => RESERVED,
    "integer" => INTEGER,
    "join" => RESERVED,
    "laplace_nd" => RESERVED,
    "laplace_np" => RESERVED,
    "laplace_zd" => RESERVED,
    "laplace_zp" => RESERVED,
    "large" => RESERVED,
    "last_crossing" => RESERVED,
    "liblist" => RESERVED,
    "library" => RESERVED,
    "limexp" => RESERVED,
    "localparam" => RESERVED,
    "macromodule" => MACROMODULE,
    "medium" => RESERVED,
    "merged" => RESERVED,
    "module" => MODULE,
    "nand" => RESERVED,
    "nature" => NATURE,
    "negedge" => RESERVED,
    "net_resolution" => RESERVED,
    "nmos" => RESERVED,
    "noise_table" => NOISE_TABLE,
    "noise_table_log" => NOISE_TABLE_LOG,
    "nor" => RESERVED,
    "noshowcancelled" => RESERVED,
    "not" => RESERVED,
    "notif0" => RESERVED,
    "notif1" => RESERVED,
    "or" => RESERVED,
    "output" => OUTPUT,
    "parameter" => PARAMETER,
    "paramset" => PARAMSET,
    "pmos" => RESERVED,
    "posedge" => RESERVED,
    "potential" => POTENTIAL,
    "primitive" => PRIMITIVE,
    "pull0" => RESERVED,
    "pull1" => RESERVED,
    "pulldown" => RESERVED,
    "pullup" => RESERVED,
    "pulsestyle_onevent" => RESERVED,
    "pulsestyle_ondetect" => RESERVED,
    "rcmos" => RESERVED,
    "real" => REAL,
    "realtime" => REALTIME,
    "reg" => REG,
    "release" => RESERVED,
    "repeat" => REPEAT,
    "resolveto" => RESERVED,
    "rnmos" => RESERVED,
    "rpmos" => RESERVED,
    "rtran" => RESERVED,
    "rtranif0" => RESERVED,
    "rtranif1" => RESERVED,
    "scalared" => RESERVED,
    "showcancelled" => RESERVED,
    "signed" => SIGNED,
    "slew" => RESERVED,
    "small" => RESERVED,
    "specify" => RESERVED,
    "specparam" => RESERVED,
    "split" => RESERVED,
    "string" => STRING,
    "strong0" => RESERVED,
    "strong1" => RESERVED,
    "supply0" => RESERVED,
    "supply1" => RESERVED,
    "table" => RESERVED,
    "task" => RESERVED,
    "time" => TIME,
    "timer" => RESERVED,
    "tran" => RESERVED,
    "tranif0" => RESERVED,
    "tranif1" => RESERVED,
    "transition" => RESERVED,
    "tri" => RESERVED,
    "tri0" => RESERVED,
    "tri1" => RESERVED,
    "triand" => RESERVED,
    "trior" => RESERVED,
    "trireg" => RESERVED,
    "units" => UNITS,
    "unsigned" => RESERVED,
    "use" => RESERVED,
    "uwire" => RESERVED,
    "vectored" => RESERVED,
    "wait" => RESERVED,
    "wand" => RESERVED,
    "weak0" => RESERVED,
    "weak1" => RESERVED,
    "while" => WHILE,
    "white_noise" => WHITE_NOISE,
    "wire" => RESERVED,
    "wor" => RESERVED,
    "wreal" => WREAL,
    "xnor" => RESERVED,
    "xor" => RESERVED,
    "zi_nd" => RESERVED,
    "zi_np" => RESERVED,
    "zi_zd" => RESERVED,
    "zi_zp" => RESERVED,

    "define" => M_DEFINE,
    "include" => M_INCLUDE,
    "elsif" => M_ELSIF,
    "endif" => M_ENDIF,
    "ifdef" => M_IFDEF,
    "ifndef" => M_IFNDEF,
    "undef" => M_UNDEF
    # else => M_ELSE
)

is_macro_only_keyword(m) = m in (M_DEFINE, M_INCLUDE, M_ELSIF, M_ENDIF, M_UNDEF)

function mktrie(keywords; thistt=IDENTIFIER, c=:(peekchar(l)), doconsume=true)
    function shift(keywords)
        new = Dict{Char, Vector{Pair{String, Kind}}}()
        add!(c, s, k) = push!(get!(new, c, Vector{Pair{String, Kind}}()), s=>k)
        foreach(((k,v),)->!isempty(k) && add!(k[1], k[2:end], v), keywords)
        new
    end

    function adjust_tt(tt)
        is_macro_only_keyword(tt) && return :(l.last_token == BACKTICK ? $tt : IDENTIFIER)
        tt == ELSE && return :(l.last_token == BACKTICK ? M_ELSE : $tt)
        return tt
    end

    cases = map(collect(shift(keywords))) do (c, vs)
        if length(vs) == 1
            kw, tt = vs[1]
            ss = isempty(kw) ? () : kw
            return c=>:(tryread(l, $ss, $(adjust_tt(tt))))
        end
        empty_idx = findfirst(x->isempty(x[1]), vs)
        return c=>mktrie(vs, thistt = empty_idx === nothing ?
            IDENTIFIER : vs[empty_idx][2])
    end

    ret = block = Expr(:block, :(
        lc = $c
    ))
    first = true
    for (c, case) in cases
        e = Expr(first ? :if : :elseif,
            :(lc == $c),
            Expr(:block, doconsume ? :(readchar(l)) : nothing, case)
        )
        push!(block.args, e)
        block = e
        first = false
    end
    push!(block.args, :(return _doret(l, lc, $(adjust_tt(thistt)))))
    ret
end

macro mktrie(keywords)
    esc(mktrie(collect(keywords), c=:c, doconsume=false))
end

# XXX: The number of keywords here is quite large, maybe
# and actual trie would be better than this generated one
@eval function lex_identifier(l, c)
    @mktrie $reserved_words
end
