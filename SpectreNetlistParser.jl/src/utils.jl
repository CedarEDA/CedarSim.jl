using Base.Meta

function mkcond(cond)
    if isexpr(cond, :call) && cond.args[1] == :(|)
        return Expr(:(||),
            mkcond(cond.args[2]),
            mkcond(cond.args[3]))
    else
        return :(cond == $(esc(cond)))
    end
end

macro case(cond, cases)
    block = ret = Expr(:block, Expr(:(=), :cond, esc(cond)))
    done = false
    first = true
    for case in cases.args
        isa(case, LineNumberNode) && continue
        isa(case, Expr) || error("Expected Expr, got $(typeof(case))a")
        done && error("Extra statements after catchall")
        if isexpr(case, :call) && case.args[1] == :(=>)
            casecond = case.args[2]
            casestmt = case.args[3]
            if casecond == :(_)
                push!(block.args, esc(casestmt))
                done = true
                continue
            end
            stmt = Expr(first ? :if : :elseif,
                mkcond(casecond),
                esc(casestmt))
            push!(block.args, stmt)
            block = stmt
            first = false
        else
            error("Unknown stmt kind $(case)")
        end
    end
    ret
end
