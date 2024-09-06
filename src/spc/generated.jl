function reload()
    @eval function (var"*self#"::SpCircuit)(nets...)
        $(Expr(:meta, :generated_only))
        $(Expr(:meta, :generated, generate_sp_code))
    end
end
reload()
