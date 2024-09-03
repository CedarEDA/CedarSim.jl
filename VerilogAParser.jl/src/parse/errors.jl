using VT100
using VT100: cmove
using Base.Iterators: reverse, rest
using AbstractTrees: parent, TreeCursor

struct CaretAnnotation
    pos::Union{VirtPos, UInt32}
    inline_annotation::String
    caret_annotation::String
    back_affinity::Bool
end
function Base.intersect(ca::CaretAnnotation, r::AbstractRange)
    if ca.back_affinity && last(r) == ca.pos
        return ca.pos:ca.pos
    end
    return intersect(ca.pos:ca.pos, r)
end

function needs_render_in(ca::CaretAnnotation, r::AbstractRange)
    return ca.back_affinity ? (first(r) < ca.pos <= last(r) + 1) : (first(r) <= ca.pos <= last(r))
end

function rerange(r::AbstractRange, ca::CaretAnnotation)
    @assert length(r) == 1
    CaretAnnotation(first(r), ca.inline_annotation, ca.caret_annotation, ca.back_affinity)
end

const VirtRange = typeof(VirtPos(0):VirtPos(1))
const PhysRange = typeof(UInt32(0):UInt32(1))

struct HighlightAnnotation
    range::Union{VirtRange, PhysRange}
    formatting::VT100.Cell
end
Base.intersect(ha::HighlightAnnotation, r::AbstractRange) = intersect(ha.range, r)
function rerange(r::AbstractRange, ha::HighlightAnnotation)
    HighlightAnnotation(r, ha.formatting)
end

struct PipeAnnotation
    range::Union{VirtRange, PhysRange}
    formatting::VT100.Cell
    label::String
end
Base.intersect(pa::PipeAnnotation, r::AbstractRange) = intersect(pa.range, r)
function rerange(r::AbstractRange, pa::PipeAnnotation)
    PipeAnnotation(r, pa.formatting, pa.label)
end

needs_render_in(a::Union{PipeAnnotation, CaretAnnotation, HighlightAnnotation}, r::AbstractRange) =
    !isempty(intersect(a, r))

struct RangeLineIterator
    leaves
end

colors = [
    #:light_red,
    :light_green,
    :cyan,
    :light_yellow,
    :magenta,
    :light_blue,
    :light_magenta,
    :light_cyan,
]

function render_pipe_annotations!(screen::ScreenEmulator, annotations::Vector{PipeAnnotation}, pos_to_col, range)
    sort!(annotations, by=x->first(x.range))

    rangewidth(irange) = pos_to_col[last(irange)] - pos_to_col[first(irange)] + 1
    awidth = map(a->rangewidth(intersect(a.range, range)), annotations)
    linepos(vp) = pos_to_col[vp]
    apos = map(a->linepos(first(intersect(a.range, range))), annotations)

    screens = map(annotations) do ann
        ascreen = ScreenEmulator()
        VT100.set_cur_cell!(ascreen, ann.formatting)
        parseall!(ascreen, IOBuffer(ann.label))
        ascreen
    end

    screenwidths = Vector{Int}(undef, 0)
    aranges = Vector{UnitRange{Int}}(undef, 0)

    for (ascreen, ann, aw, ap) in zip(screens, annotations, awidth, apos)
        screen[2, ap:(ap+aw-1)] .= ann.formatting('━')
        arange = intersect(ann.range, range)
        if first(arange) != first(ann.range)
            screen[2, ap] = ann.formatting('┅')
        end
        if last(arange) != last(ann.range)
            screen[2, ap+aw-1] = ann.formatting('┅')
        end
        push!(screenwidths, length(ascreen.lines[1]))
        push!(aranges, ap:(ap+aw-1))
    end

    allocated_lines = fill(0, length(annotations))
    allocated_all = false
    cur_line = 3
    while !allocated_all
        # Go line by line, greedly allocating space for screens
        lastend = 0
        allocated_any = false
        force = false
@label retry
        allocated_all = true
        for i = 1:length(annotations)
            allocated_lines[i] == 0 || continue
            # Check if there is space for this screen
            swidth = screenwidths[i]
            arange = aranges[i]
            next_unallocated_ann = findnext(==(0), allocated_lines, i + 1)
            next_arange = next_unallocated_ann === nothing ? (size(screen, 2):(size(screen, 2) - 1)) : aranges[next_unallocated_ann]
            available_range = (lastend+1):last(next_arange)

            if swidth <= length(available_range) || force
                preferred_start = apos[i]+div(length(arange), 2)-div(swidth,2)

                if preferred_start <= lastend + 1
                    preferred_start = lastend + 1
                elseif preferred_start + swidth > first(next_arange)
                    nspaces = preferred_start - (lastend + 1)
                    nspaces -= (preferred_start + swidth - first(next_arange))
                    nspaces = max(nspaces, 0)
                    preferred_start = lastend + 1 + div(nspaces, 2)
                end
                # Render this line
                cmove(screen, cur_line, preferred_start)
                for c in screens[i].lines[1]
                    write(screen, c)
                end

                # TODO: Draw lines connecting annotation to label, avoiding labels on previous lines
                overlap_range = intersect(preferred_start:(preferred_start+swidth-1), arange)
                arrow_pos = first(overlap_range) + div(length(overlap_range), 2)
                screen[2, arrow_pos] = annotations[i].formatting(arrow_pos == last(arange) ? '┓' :
                                                                 arrow_pos == first(arange) ? '┏' :
                                                                 '┳')

                lastend = preferred_start + swidth - 1
                allocated_any = true
                allocated_lines[i] = cur_line
            else
                allocated_all = false
            end
        end

        if !allocated_all && !allocated_any
            if !force
                force = true
                @goto retry
            end
            @warn "Unable to print all errors"
            break
        end

        cur_line += 1
    end
end

function render_highlight_annotations!(screen::ScreenEmulator, annotations::Vector{HighlightAnnotation}, pos_to_col, range)
    rangewidth(irange) = pos_to_col[last(irange)] - pos_to_col[first(irange)] + 1
    awidth = map(a->rangewidth(intersect(a.range, range)), annotations)
    linepos(vp) = pos_to_col[vp]
    apos = map(a->linepos(first(intersect(a.range, range))), annotations)

    for (ann, aw, ap) in zip(annotations, awidth, apos)
        screen[1, ap:(ap+aw-1)] .= map(c->ann.formatting(c.content), screen[1, ap:(ap+aw-1)])
    end
end

function render_caret_annotations!(screen::ScreenEmulator, annotations::Vector{CaretAnnotation}, pos_to_col, range)
    linepos(vp) = pos_to_col[vp]
    apos = map(a->linepos(a.pos), annotations)

    for (ann, ap) in zip(annotations, apos)
        cmove(screen, 2, ap+1)
        VT100.set_cur_cell!(screen, Cell(fg=:green))
        parseall!(screen, IOBuffer(ann.caret_annotation))
    end
end

function add_gutter!(screen::ScreenEmulator, lno::Int, isexpansion::Bool, iswrap::Bool; max_lno_width=length(string(lno)))
    lno_string = string(lno)
    for row in 1:length(screen.lines)
        gutter = Cell[]
        push!(gutter, Cell(Cell('│'), fg=:red))
        if row == 1
            if isexpansion
                push!(gutter, Cell(Cell('↔'), fg=:dark_gray))
                append!(gutter, Cell() for _ in 1:max_lno_width)
            else
                append!(gutter, Cell(Cell(c), fg=:dark_gray) for c in lno_string)
                append!(gutter, Cell() for _ in 1:(max_lno_width - sizeof(lno_string)))
                push!(gutter, iswrap ? Cell(Cell('⤾'), fg=:dark_gray) : Cell())
            end
        else
            append!(gutter, Cell() for _ in 1:(max_lno_width + 1))
        end
        push!(gutter, Cell(Cell(row == 1 ? '│' : '┊'), fg=:dark_gray))
        push!(gutter, Cell())
        splice!(screen.lines[row].data, 1:0, gutter)
    end
end

Base.iterate(ri::RangeLineIterator, ::Nothing) = nothing
function Base.iterate(rli::RangeLineIterator, (t, vro, i, o) = (String(rli.leaves[1]), first(virtrange(rli.leaves[1])), 1, 1))
    nextnl = findnext('\n', t, o)
    #@show (o, nextnl)
    if nextnl !== nothing
        len = nextnl-o
        #@show (vro, o, len, nextnl)
        #@show t[o:(nextnl-1)]
        return (vro:(vro+len-1), t[o:(nextnl-1)]), (t, vro+len+1, i, nextnl+1)
    end

    i == length(rli.leaves) && @goto end_of_text

    nextt = String(rli.leaves[i+1])
    len = sizeof(t) - o + 1
    r = iterate(rli, (nextt, vro+len, i+1, 1))
    r === nothing && @goto end_of_text

    return (vro:last(r[1][1]), string(t[o:end], r[1][2])), r[2]

@label end_of_text
    o === sizeof(t) && return nothing
    len = sizeof(t) - o
    return (vro:(vro+len-1), t[o:end]), nothing

end

function render_annotated_snippet(io, file_leaf, leaves, annotations, contextrange, cur_lno; nlines=nothing, max_lno_width, macro_color_assignment=nothing)
    guttersize = 4 + length(string(cur_lno))
    sz = displaysize(io)
    (width, height) = sz[2] - guttersize, sz[1]

    allcanns = filter(a->isa(a, CaretAnnotation), annotations)
    sort!(allcanns, by=a->a.pos)

    # TOOD: Better data structure for this
    pos_to_col = Dict{VirtPos, Int}()

    all_leaves = Iterators.Stateful(leaves)

    screen = ScreenEmulator(width, height)
    first_leaf = true
    iswrap = false
    isexpansion = false
    while !isempty(all_leaves) && (nlines === nothing || nlines > 0)
        tc = popfirst!(all_leaves)
        leaf = nodevalue(tc)
        lrange = virtrange(leaf)
        thisrange = lrange
        text = String(leaf)

        # We only truncate the first leaf. It is possible that collect_leaves
        # gave us more leaves than are necessary to cover the contextrange in
        # order to anchor the macro expansion in a file. We always want to
        # make sure to get at least one line from the original file at the
        # start of the expansion.
        if first_leaf && first(thisrange) < first(contextrange)
            for (offset, c) in pairs(text)
                if c == '\n'
                    if offset - 1 > (first(contextrange) - first(lrange))
                        break
                    end
                    # N.B.: No -1 here to move *past* the current \n
                    thisrange = (first(lrange) + offset):last(thisrange)
                end
            end
            first_leaf = false
        end

        nn = nodevalue(parent(tc))
        is_any_macro = is_macro_expansion(nn)
        is_macro = is_any_macro && !nn.offsets[nn.chunk_idx].macro_expansion.def.is_formal_arg
        if is_macro
            macro_color = macro_color_assignment[nn.offsets[nn.chunk_idx].macro_expansion.def]
        end

        if leaf.expansion
            thisrange = 1:sizeof(text)
        end

        @label keep_going

        if !is_any_macro
            isexpansion = false
        end

        if !leaf.expansion && first(thisrange) > last(contextrange)
            break
        end

        if leaf.expansion
            thisrange_text = text
        else
            shift = UInt32(first(virtrange(leaf))) - 1
            viewrange = (UInt32(first(thisrange)) - shift):(UInt32(last(thisrange)) - shift)
            thisrange_text = @view text[viewrange]
        end

        inc_nl = false

        theoffset = 0
        is_macro && VT100.set_cur_cell!(screen, Cell(attrs = VT100.Attributes.Underline))
        for (offset, c) in pairs(thisrange_text)
            theoffset = offset
            thispos = first(thisrange) + offset - 1
            if c == '\n'
                if leaf.fidx == file_leaf.fidx && !is_any_macro
                    inc_nl = true
                end
            else
                if is_macro && c == '\\' && offset + 1 <= sizeof(thisrange_text) && thisrange_text[offset+1] == '\n'
                    # Line continuation character; skip it for visual cleanliness
                else
                    write(screen, is_macro ? Cell(Cell(c), fg=macro_color) : Cell(c))
                end
            end
            if !leaf.expansion
                pos_to_col[thispos] = length(screen.lines[1])
            end
            if !isempty(allcanns)
                fcann = first(allcanns)
                if thispos == fcann.pos
                    @assert !leaf.expansion
                    fcw = textwidth(fcann.inline_annotation)
                    if fcw < width && (fcw + length(screen.lines[1])) > width
                        @goto end_of_line
                    end
                    popfirst!(allcanns)
                    old_cell = VT100.cur_cell(screen)
                    VT100.set_cur_cell!(screen, Cell(fg=:light_red))
                    parseall!(screen, IOBuffer(fcann.inline_annotation))
                    VT100.set_cur_cell!(screen, old_cell)
                end
            end
            if length(screen.lines[1]) == width || c == '\n'
                @goto end_of_line
            end
        end

        if !isempty(all_leaves)
            continue
        end

        @label end_of_line
        # We're guaranteed no annotations in non-expanded leafs
        if !leaf.expansion
            pos_to_col[first(thisrange) + theoffset] = length(screen.lines[1])
            renderrange = first(thisrange):(first(thisrange) + theoffset - 1)
            anns = [a for a in annotations if needs_render_in(a, renderrange)]
            panns = PipeAnnotation[a for a in anns if isa(a, PipeAnnotation)]
            render_pipe_annotations!(screen, panns, pos_to_col, renderrange)
            hanns = HighlightAnnotation[a for a in anns if isa(a, HighlightAnnotation)]
            render_highlight_annotations!(screen, hanns, pos_to_col, renderrange)
            canns = CaretAnnotation[a for a in anns if isa(a, CaretAnnotation)]
            render_caret_annotations!(screen, canns, pos_to_col, renderrange)
        end

        add_gutter!(screen, cur_lno, isexpansion, iswrap; max_lno_width)
        VT100.render(io, screen)
        println(io)
        screen = ScreenEmulator(width, height)

        if inc_nl
            cur_lno += 1
            nlines !== nothing && (nlines -= 1)
            iswrap = false
        else
            iswrap = true
        end

        nlines == 0 && break

        isexpansion = true

        thisrange = (first(thisrange) + theoffset):last(thisrange)
        isempty(thisrange) || @goto keep_going
    end
end

pathname(sf) = sf.path === nothing ? "<input>" : basename(sf.path)

function collect_leaves!(leaves, macros, ps, contextrange, annotations, tc, is_in_macro_expansion = false)
    for tc′ in children(tc)
        leaf = nodevalue(tc′)
        if contextrange ⊆ virtrange(leaf)
            if leaf.expansion
                is_macro_expansion(leaf) && push!(macros, leaf.offsets[leaf.chunk_idx].macro_expansion.def)
                return collect_leaves!(leaves, macros, ps, contextrange, annotations, tc′, true)
            end
        elseif last(virtrange(leaf)) < first(contextrange) && !is_in_macro_expansion
            continue
        elseif first(virtrange(leaf)) > last(contextrange)
            break
        end

        # If this a macro expansion, decide whether or not to render the
        # expansion. For the moment, we only render expansions that contain
        # annotations.
        if leaf.expansion && ((is_in_macro_expansion && leaf.offsets[leaf.chunk_idx].macro_expansion.def.is_formal_arg) || any(ann->needs_render_in(ann, virtrange(leaf)), annotations))
            push!(macros, leaf.offsets[leaf.chunk_idx].macro_expansion.def)
            collect_leaves!(leaves, macros, ps, contextrange, annotations, tc′, true)
        else
            push!(leaves, tc′)
        end
    end
end

function compute_leaves2(ps, contextrange, annotations, tc=AbstractTrees.TreeCursor(ChunkTree(ps)))
    leaves = Any[]
    file_leaf = nothing

    file_annotations = Any[]
    extra_virt_annotations = Any[]

    macros = Any[]

    collect_leaves!(leaves, macros, ps, contextrange, annotations, tc)
    unique!(macros)

    if !isempty(leaves)
        file_leaf = leaves[1]
        while true
            p = AbstractTrees.parent(file_leaf)
            p === nothing || AbstractTrees.isroot(p) && break
            !is_macro_expansion(nodevalue(p)) && break
            file_leaf = p
        end
    end
    file_leaf !== nothing && (file_leaf = file_leaf)

    file_leaf, leaves, macros, file_annotations, extra_virt_annotations
end

function print_snippet(io, ps, contextrange, annotations)
    isempty(contextrange) && return

    file_leaf, leaves, macros, file_annotations, extra_virt_annotations = compute_leaves2(ps, contextrange, annotations)
    append!(annotations, extra_virt_annotations)

    isempty(leaves) && return

    if nodevalue(leaves[1]) === nodevalue(file_leaf)
        # Compute line numbers
        vr = first(virtrange(nodevalue(leaves[1]))):last(virtrange(nodevalue(leaves[end])))
        fr = first(filerange(nodevalue(leaves[1]))):last(filerange(nodevalue(leaves[end])))

        startoff_file = first(fr) + (first(contextrange) - first(vr))
    else
        startoff_file = first(filerange(nodevalue(file_leaf)))
    end

    sf = ps.srcfiles[nodevalue(leaves[1]).fidx]
    lsf = sf.lineinfo
    lno_first = compute_line(lsf, startoff_file)
    lno_last = compute_line(lsf, last(filerange(nodevalue(file_leaf))))
    max_lno_width = sizeof(string(lno_last))

    cur_lno = lno_first

    guttersize = 4 + max_lno_width
    sz = displaysize(io)
    (width, height) = sz[2] - guttersize, sz[1]

    if width < 15
        print(io, pathname(sf) , ":", cur_lno)
        printstyled(io, "Narrow Terminal"; color=:red)
        println(io)
        return
    end

    firstit = true

    the_file_leaf = nodevalue(file_leaf)

    path = string(pathname(sf) , ":", lno_first)
    macro_color_assignment = Dict{MacroDef, Symbol}(mac => color for (mac, color) in
        zip(Iterators.filter(!mac->mac.is_formal_arg, macros), Iterators.cycle(colors)))
    if !isempty(macros)
        printstyled(io, "│", color=:red)
        printstyled(io, string(" "^(guttersize-3), "╭───── ", pathname(sf) , ":", lno_first), color=:light_black)
        println(io)

        phys_leafs = Any[file_leaf]

        # Make sure to add the entire line
        this_leaf = file_leaf
        while !('\n' in String(nodevalue(this_leaf)))
            this_leaf = AbstractTrees.prevsibling(this_leaf)
            pushfirst!(phys_leafs, this_leaf)
        end

        this_leaf = file_leaf
        while true
            next_leaf = AbstractTrees.nextsibling(this_leaf)
            next_leaf === nothing && break
            first(virtrange(nodevalue(next_leaf))) > last(virtrange(nodevalue(leaves[end]))) && break
            push!(phys_leafs, next_leaf)
            this_leaf = next_leaf
        end

        render_annotated_snippet(io, the_file_leaf, phys_leafs, Any[], contextrange, cur_lno; max_lno_width)

        for mac in macros
            mac.is_formal_arg && continue

            _, macro_leaves, _, _, _ = compute_leaves2(ps, mac.defrange, Any[])

            mac_leaf = nodevalue(macro_leaves[1])
            mac_sf = ps.srcfiles[mac_leaf.fidx]
            mac_lsf = LineNumbers.SourceFile(mac_sf.contents.data)
            mac_lno_first = compute_line(mac_lsf, (first(mac.defrange) - first(virtrange(mac_leaf))) + first(filerange(mac_leaf)))

            path = string(pathname(mac_sf) , ":", mac_lno_first)
            printstyled(io, "│", color=:red)
            printstyled(io, string(" "^(guttersize-3),"├───── Using macro "), color=:light_black)
            printstyled(io, string("`", mac.name), color=macro_color_assignment[mac])
            printstyled(io, string(" at ", path), color=:light_black)
            println(io)

            render_annotated_snippet(io, nodevalue(macro_leaves[1]), macro_leaves, Any[], mac.defrange, mac_lno_first; nlines=2, max_lno_width)
        end
        firstit = false
    end


    if !firstit
        path = "Expanded to"
    end

    printstyled(io, "│", color=:red)
    c = firstit ? "╭" : "├"
    printstyled(io, string(" "^(guttersize-3),"$(c)───── ", path), color=:light_black)
    println(io)

    render_annotated_snippet(io, the_file_leaf, leaves, annotations, contextrange, cur_lno; max_lno_width, macro_color_assignment)

    printstyled(io, "│", color=:red)
    printstyled(io, string(" "^(guttersize-3), "│"), color=:light_black)
    println(io)
    printstyled(io, "│", color=:red)
    printstyled(io, string(" "^(guttersize-3), "╰───── "), color=:light_black)
end

function print_help(io, label)
    guttersize = 8
    sz = displaysize(io)
    (width, height) = sz[2] - guttersize, sz[1]
    screen = ScreenEmulator(width, height)

    parseall!(screen, IOBuffer(label))

    for row in 1:length(screen.lines)
        gutter = Cell[]
        push!(gutter, Cell(Cell('│'), fg=:red))
        push!(gutter, Cell())
        append!(gutter, Cell(row == 1 ? Cell(c) : Cell(), fg=:blue) for c in "help:")
        push!(gutter, Cell())
        splice!(screen.lines[row].data, 1:0, gutter)
    end

    VT100.render(io, screen)
end

function add_block_matching!(annotations, stmts::NodeList, colors=Iterators.Stateful(Iterators.cycle(colors)))
    for stmt′ in stmts
        add_block_matching!(annotations, stmt′, colors)
    end
    return
end


function add_block_matching!(annotations, stmt, colors=Iterators.Stateful(Iterators.cycle(colors)))
    if isa(stmt, Node{AnalogSeqBlock})
        formatting = Cell(fg = popfirst!(colors))
        push!(annotations, HighlightAnnotation(spanvirtrange(stmt.kwbegin), formatting))
        push!(annotations, HighlightAnnotation(spanvirtrange(stmt.kwend), formatting))
        add_block_matching!(annotations, stmt.stmts, colors)
        return
    else
        for stmt′ in children(stmt)
            add_block_matching!(annotations, stmt′, colors)
        end
        return
    end
end

import AbstractTrees: IteratorState, LeavesState

struct WrappedPreOrderDFS{T} <: TreeIterator{T}
    root::T
end

AbstractTrees.statetype(itr::WrappedPreOrderDFS) = AbstractTrees.PreOrderState

function Base.iterate(ti::WrappedPreOrderDFS, s::Union{Nothing,IteratorState}=AbstractTrees.initial(AbstractTrees.statetype(ti), ti.root))
    isnothing(s) && return nothing
    (s.cursor, AbstractTrees.next(s))
end

function visit_errors(va; io=stdout, verbose=false)
    #va.ps.errored || return true
    vatc = TreeCursor(va)
    prevleaf = AbstractTrees.descendleft(va)
    for tc in WrappedPreOrderDFS(vatc)
        child = nodevalue(tc)
        if isa(child, Node{Error}) || isa(child, Node{MacroError})
            theerror = child.expr
            contextrange = virtrange(child)
            p = AbstractTrees.parent(child)
            while true
                if isa(p, Node{ModuleItem}) || isa(p, Node{AnalogStatement})
                    contextrange = spanvirtrange(p)
                    break;
                end
                pp = AbstractTrees.parent(p)
                if pp === nothing
                    contextrange = spanvirtrange(p)
                    break;
                end
                p = pp
            end
            if verbose
                @show p
                dump(theerror)
            end
            annotation = nothing
            if theerror.kind == UnexpectedToken
                context = theerror.context === nothing ? () :
                          isa(theerror.context, Kind) ? (theerror.context,) :
                          theerror.context
                if theerror.got==PREPROC_ERR_MISSING_MACRO
                    printstyled(io, "╭ ERROR [VAS02]:", bold=true, color=:red)
                    printstyled(io, " Use of undefined macro.", color=:red)
                    println(io)
                    mname = String(child)
                    annotations = [
                        PipeAnnotation(spanvirtrange(child), Cell(fg = :light_red), String("✗ Undefined Macro"));
                        HighlightAnnotation(spanvirtrange(child), Cell(fg = :light_red))]
                    print_snippet(io, va.ps, contextrange, annotations)
                    print(io, "Use of undefined macro ")
                    printstyled(io, mname, color=:default, bold=true)
                    println(io, ".")
                    printstyled(io, "│", color=:red)
                    println(io)
                    printstyled(io, "│", color=:red)
                    printstyled(io, " help: ", color=:blue)
                    print(io, "The code made use of the macro ")
                    printstyled(io, mname, color=:default, bold=true)
                    print(io, ". However, this macro was not defined.")
                    println(io)
                    printstyled(io, "│", color=:red)
                    print(io, "       ", "Check for spelling mistakes or definition ordering.")
                    println(io)
                    printstyled(io, "╯", color=:red)
                    println(io)
                    return false
                elseif any(==(SEMICOLON), context)
                    annotations = Any[]
                    printstyled(io, "╭ ERROR [VAS47]:", bold=true, color=:red)
                    printstyled(io, " Expected semicolon", color=:red)
                    if isa(va, Node{VerilogModule})
                        note = " after port list in module declaration."
                    elseif isa(va, Node{ModuleItem}) && isa(va.item, Node{ParameterDeclaration})
                        note = " after end of parameter declaration."
                        push!(annotations, PipeAnnotation(spanvirtrange(va.item.kw), Cell(fg = :dark_gray), "Declaration starts here"))
                    else
                        note = "."
                    end
                    printstyled(io, note, color=:red)
                    println(io)

                    contextrange = first(spanvirtrange(parent(child))):last(spanvirtrange(prevleaf))
                    annotation = CaretAnnotation(last(spanvirtrange(prevleaf))+1, "◌ ", ";", true)

                    push!(annotations, annotation)

                    print_snippet(io, va.ps, contextrange, annotations)

                    print(io, " Missing semicolon", note)
                    println(io); printstyled(io, "│", color=:red)
                    println(io)

                    print_help(io, string(
                        "The parser considered the declaration complete at the ",
                        "indicated location and thus expected a semicolon. ",
                        "If you expected the subsequent code to be part of this ",
                        "declaration, check for missing syntax (e.g. binary operators)."))
                    println(io)
                    printstyled(io, "╯", color=:red)

                    return false
                elseif theerror.got === SEMICOLON
                    printstyled(io, "╭ ERROR [VAS46]:", bold=true, color=:red)
                    printstyled(io, " Unexpected semicolon.", color=:red)
                    println(io)

                    annotations = Any[
                        HighlightAnnotation(spanvirtrange(child), Cell(fg = :light_red)),
                        PipeAnnotation(spanvirtrange(child), Cell(fg = :light_red), String("✗ Unexpected semicolon"))
                    ]

                    contextrange = first(spanvirtrange(parent(prevleaf))):last(spanvirtrange(child))

                    print_snippet(io, va.ps, contextrange, annotations)
                    println(io, "Unexpected semicolon.")
                    return false
                elseif theerror.got !== nothing && isoperator(theerror.got)

                    printstyled(io, "╭ ERROR [VAS99]:", bold=true, color=:red)
                    printstyled(io, " Unexpected operator.", color=:red)
                    println(io)

                    pnode = parent(child)
                    contextrange = first(spanvirtrange(pnode)):last(virtrange(child))
                    while isempty(contextrange)
                        pnode = parent(pnode)
                        contextrange = first(spanvirtrange(pnode)):last(virtrange(child))
                    end

                    annotations = Any[PipeAnnotation(spanvirtrange(child), Cell(fg = :light_red), String("✗ Unexpected operator: "))]

                    print_snippet(io, va.ps, contextrange, annotations)
                    println(io)
                    printstyled(io, "╯", color=:red)
                    return false
                elseif any(==(IDENTIFIER), context)
                    annotations = Any[]
                    printstyled(io, "╭ ERROR [VAS48]:", bold=true, color=:red)
                    printstyled(io, " Expected identifier.", color=:red)
                    println(io)

                    @show theerror.got

                    pnode = parent(child)
                    while isa(pnode, Union{Node{Identifier}, Node{<:ListItem}})
                        pnode = parent(pnode)
                    end

                    contextrange = first(spanvirtrange(pnode)):first(virtrange(child))-1
                    while isempty(contextrange)
                        pnode = parent(pnode)
                        contextrange = first(spanvirtrange(pnode)):first(virtrange(child))-1
                    end
                    annotation = CaretAnnotation(first(virtrange(child)), "◌ ", "", true)
                    push!(annotations, annotation)

                    print_snippet(io, va.ps, contextrange, annotations)

                    if isa(pnode, Node{NetDeclaration})
                        print(io, "Expected an identifier while parsing this net declaration.")
                    end
                    println(io); printstyled("│", color=:red)
                    println(io)

                    print_help(io, string(
                        "The parser expected an identifier at the indicated location."))
                    println(io)
                    printstyled(io, "╯", color=:red)

                    return false
                elseif any(x->x == END || x == ENDFUNCTION || x == ENDMODULE, context)
                    annotations = Any[]

                    contextrange = first(contextrange):max(first(virtrange(child))+1, last(contextrange))

                    if theerror.got === END
                        verb = "Incorrectly terminated"
                        push!(annotations,
                            PipeAnnotation(spanvirtrange(child), Cell(fg = :light_red), String("✗ Incorrect Termination")))
                    else
                        verb = "Unterminated"
                        push!(annotations,
                            CaretAnnotation(first(virtrange(child))-1, "◌ ", "end", true))
                    end

                    if isa(parent(child), Node{AnalogSeqBlock})
                        push!(annotations, PipeAnnotation(spanvirtrange(parent(child).kwbegin), Cell(fg = :yellow), "Block starts here"))
                        printstyled(io, "╭ ERROR [VAS49]:", bold=true, color=:red)
                        printstyled(io, " $verb Analog Block.", color=:red)
                        add_block_matching!(annotations, parent(child).stmts)
                    elseif isa(parent(child), Node{AnalogFunctionDeclaration})
                        push!(annotations, PipeAnnotation(spanvirtrange(parent(child).fkw), Cell(fg = :yellow), "Function declaration starts here"))
                        printstyled(io, "╭ ERROR [VAS50]:", bold=true, color=:red)
                        printstyled(io, " $verb Analog Function.", color=:red)
                        add_block_matching!(annotations, parent(child).stmt)
                    elseif isa(parent(child), Node{VerilogModule})
                        push!(annotations, PipeAnnotation(spanvirtrange(parent(child).kw), Cell(fg = :yellow), "Module declaration starts here"))
                        printstyled(io, "╭ ERROR [VAS51]:", bold=true, color=:red)
                        printstyled(io, " $verb Verilog Module.", color=:red)
                        add_block_matching!(annotations, parent(child).items)
                    end

                    println(io)

                    print_snippet(io, va.ps, contextrange, annotations)

                    return false
                elseif theerror.got == ANALOG
                    printstyled(io, "╭ ERROR [VAS25]:", bold=true, color=:red)
                    printstyled(io, " Unexpected analog keyword.", color=:red)
                    println(io)

                    annotations = Any[
                        PipeAnnotation(spanvirtrange(child), Cell(fg = :light_red), String("✗ Illegal Analog Keyword"))
                    ]

                    print_snippet(io, va.ps, contextrange, annotations)
                    return false
                elseif isa(parent(parent(child)), Node{CaseItem})
                    annotations = Any[]

                    if theerror.got == BEGIN
                        errmsg = " begin/end blocks not allowed inside case statements."
                        errstr = "✗ Unexpected `begin`"
                    else
                        errmsg = "Unexpected token inside case stement. Expected condition or `default`."
                        errstr = "✗ Unexpected token"
                    end

                    printstyled(io, "╭ ERROR [VAS26]:", bold=true, color=:red)
                    printstyled(io, errmsg, color=:red)
                    println(io)

                    pnode = parent(child)
                    isa(pnode, Node{ListItem}) && (pnode = parent(pnode))

                    contextrange = first(spanvirtrange(pnode)):last(spanvirtrange(child))

                    push!(annotations, PipeAnnotation(spanvirtrange(child), Cell(fg = :light_red), String(errstr)))

                    case = parent(parent(parent(child)))
                    push!(annotations, PipeAnnotation(spanvirtrange(case.kw), Cell(fg = :yellow), "`case` statement starts here"));

                    print_snippet(io, va.ps, contextrange, annotations)
                    println(io)
                    printstyled(io, "╯", color=:red)
                    return false
                elseif any(==(LPAREN), context)
                    annotations = Any[]

                    contextrange = first(spanvirtrange(parent(child))):first(virtrange(child))-1
                    annotation = CaretAnnotation(first(virtrange(child))-1, "◌ ", "(", true)
                    push!(annotations, annotation)

                    errmsg = " Missing opening parenthesis."

                    pnode = parent(child)
                    if isa(pnode, Node{AnalogIf})
                        if isa(pnode.rparen, Node{Error})
                            errmsg = " Missing parentheses."
                            push!(annotations, CaretAnnotation(first(virtrange(pnode.rparen))-1, "◌ ", ")", true))
                        else
                            push!(annotations, HighlightAnnotation(spanvirtrange(pnode.rparen), Cell(fg = :yellow)))
                            push!(annotations, PipeAnnotation(spanvirtrange(pnode.rparen), Cell(fg = :yellow), "to match `)` here"))
                        end
                    end

                    printstyled(io, "╭ ERROR [VAS27]:", bold=true, color=:red)
                    printstyled(io, errmsg, color=:red)
                    println(io)

                    print_snippet(io, va.ps, contextrange, annotations)
                    println(io)
                    printstyled(io, "╯", color=:red)
                    return false
                elseif isa(parent(child), Node{ModuleItem})
                    annotations = Any[]
                    printstyled(io, "╭ ERROR [VAS28]:", bold=true, color=:red)
                    printstyled(io, " Unexpected token in module scope.", color=:red)
                    println(io)

                    pnode = parent(child)
                    contextrange = first(spanvirtrange(pnode)):last(virtrange(child))

                    pl = nodevalue(AbstractTrees.prevsibling(parent(tc)))
                    if theerror.got in (BEGIN, IF, END) # TODO: Other things that indicate this was meant to be in the analog block?
                        if isa(pl, Node{ModuleItem})
                            if isa(pl.item, Node{AnalogBlock})
                                block = pl.item
                                @show typeof(block.stmt)
                                if isa(block.stmt, Node{AnalogStatement}) && isa(block.stmt.stmt, Node{AnalogSeqBlock})
                                    asb = block.stmt.stmt
                                    push!(annotations, PipeAnnotation(spanvirtrange(asb.kwbegin), Cell(fg = :yellow), String("Previous analog block began here")))
                                    push!(annotations, PipeAnnotation(spanvirtrange(asb.kwend), Cell(fg = :yellow), String("Previous analog block ended here")))
                                    add_block_matching!(annotations, asb.stmts)
                                    contextrange = first(spanvirtrange(asb)):last(contextrange)
                                end
                            end
                        end
                    end

                    while isempty(contextrange)
                        pnode = parent(pnode)
                        contextrange = first(spanvirtrange(pnode)):last(virtrange(child))
                    end

                    errstr = String("✗ Unexpected token")
                    if theerror.got == BEGIN
                        errstr = String("✗ Unexpected `begin` at module scope")
                    end

                    if isempty(virtrange(child))
                        annotation = CaretAnnotation(first(virtrange(child))-1, "◌ ", "", true)
                    else
                        annotation = PipeAnnotation(spanvirtrange(child), Cell(fg = :light_red), errstr)
                    end
                    push!(annotations, annotation)

                    print_snippet(io, va.ps, contextrange, annotations)
                    println(io)
                    printstyled(io, "╯", color=:red)
                    return false
                else
                    annotations = Any[]
                    printstyled(io, "╭ ERROR [VAS??]:", bold=true, color=:red)
                    printstyled(io, " Unexpected token (please ask for a better error message).", color=:red)
                    println(io)

                    @show theerror.got
                    @show typeof(parent(child))

                    pnode = parent(child)
                    contextrange = first(spanvirtrange(pnode)):last(virtrange(child))
                    while isempty(contextrange)
                        pnode = parent(pnode)
                        contextrange = first(spanvirtrange(pnode)):last(virtrange(child))
                    end

                    if isempty(virtrange(child))
                        annotation = CaretAnnotation(first(virtrange(child))-1, "◌ ", "", true)
                    else
                        annotation = PipeAnnotation(spanvirtrange(child), Cell(fg = :light_red), String("✗ Unexpected token"))
                    end
                    push!(annotations, annotation)

                    print_snippet(io, va.ps, contextrange, annotations)
                    println(io)
                    printstyled(io, "╯", color=:red)
                    return false
                end
            elseif theerror.kind == BadMacroCall
                printstyled(io, "╭ ERROR [VAS01]:", bold=true, color=:red)
                printstyled(io, " Macro expansion argument mismatch.", color=:red)
                println(io)
                # First find out which macro this was
                def_annotations = Any[]
                use_annotations = Any[]
                idx = 0
                arg_formatting = Dict{Symbol, Cell}()
                nactualargs = 0
                for i = 1:length(theerror.def.formal_args)
                    idx = 1 + 2i
                    formal_arg = theerror.def.formal_args[i]
                    if idx > length(child.args)
                        formatting = Cell(fg = :light_red)
                        # Annotation just before the closing paren
                        push!(use_annotations, CaretAnnotation(first(virtrange(child.args[end]))-1, ", ◌ ", String(formal_arg.name), true))
                    else
                        formatting = Cell(fg = colors[i])
                    end
                    arg_formatting[formal_arg.name] = formatting
                    push!(def_annotations,
                        HighlightAnnotation(formal_arg.namerange, formatting))
                    idx > length(child.args) && continue
                    nactualargs += 1
                    push!(use_annotations,
                        PipeAnnotation(virtrange(child.args[idx]), formatting, String(formal_arg.name)))

                end
                idx += 2
                while idx < length(child.args)
                    push!(use_annotations,
                        PipeAnnotation(virtrange(child.args[idx]), Cell(fg = :light_red), String("✗ Extra Argument")))
                    push!(use_annotations,
                        HighlightAnnotation(virtrange(child.args[idx]), Cell(fg = :light_red)))
                    idx += 2
                    nactualargs += 1
                end

                # Add highlights for uses of the message.
                for tok in theerror.def.tokens
                    if tok.kind == IDENTIFIER
                        sym = Symbol(String(resolve_identifier(va.ps, theerror.def.fidx, tok)))
                        if haskey(arg_formatting, sym)
                            vrs = last(theerror.def.defrange) - (theerror.def.tokens[end].endbyte - tok.startbyte)
                            vre = vrs + (tok.endbyte - tok.startbyte)
                            push!(def_annotations,
                                HighlightAnnotation(vrs:vre, arg_formatting[sym]))
                        end
                    end
                end

                nformalargs = length(theerror.def.formal_args)

                print_snippet(io, va.ps, contextrange, use_annotations)
                print(io, "Macro ")
                printstyled(io, theerror.def.name, color=:default, bold=true)
                print(io, " was called with ", nactualargs, " arguments.")
                println(io)
                printstyled(io,"│", color=:red)
                println(io)
                print_snippet(io, va.ps, theerror.def.defrange, def_annotations)
                print(io, "Macro ")
                printstyled(io, theerror.def.name, color=:default, bold=true)
                print(io, " takes ", length(theerror.def.formal_args), " arguments.")
                println(io)
                printstyled(io,"│", color=:red)
                println(io)
                printstyled(io,"│", color=:red)
                printstyled(io, " help: ", color=:blue)
                print(io, "The macro ")
                printstyled(io, theerror.def.name, color=:default, bold=true)
                print(io, " was called with ", nactualargs > nformalargs ? "more" : "fewer", " arguments (",nactualargs,") than it expected (",nformalargs,").")
                println(io)
                printstyled(io,"│", color=:red)
                print(io, "       ", "Check for any misplaced commas in the macro invocation above.")
                println(io)
                printstyled(io, "╯", color=:red)
                println(io)
                return false
            elseif theerror.kind == UndefinedMacro
                printstyled(io, "╭ ERROR [VAS02]:", bold=true, color=:red)
                printstyled(io, " Use of undefined macro.", color=:red)
                println(io)
                mname = String(child)
                annotations = [
                    PipeAnnotation(spanvirtrange(child), Cell(fg = :light_red), String("✗ Undefined Macro"));
                    HighlightAnnotation(spanvirtrange(child), Cell(fg = :light_red))]
                print_snippet(io, va.ps, contextrange, annotations)
                print(io, "Use of undefined macro ")
                printstyled(io, mname, color=:default, bold=true)
                println(io, ".")
                printstyled(io, "│", color=:red)
                println(io)
                printstyled(io, "│", color=:red)
                printstyled(io, " help: ", color=:blue)
                print(io, "The code made use of the macro ")
                printstyled(io, mname, color=:default, bold=true)
                print(io, ". However, this macro was not defined.")
                println(io)
                printstyled(io, "│", color=:red)
                print(io, "       ", "Check for spelling mistakes or definition ordering.")
                println(io)
                printstyled(io, "╯", color=:red)
                println(io)
                return false
            elseif theerror.kind == MissingMacro
                printstyled(io, "╭ ERROR [VAS03]:", bold=true, color=:red)
                printstyled(io, " Use of backtick not followed by a macro name.", color=:red)
                println(io)
                annotations = [
                    PipeAnnotation(spanvirtrange(child), Cell(fg = :light_red), String("Missing macro name"));
                    HighlightAnnotation(spanvirtrange(child), Cell(fg = :light_red))]
                print_snippet(io, va.ps, contextrange, annotations)
                println(io, "Use of backtick not followed by a macro name.")
                printstyled(io, "│", color=:red)
                println(io)
                printstyled(io, "│", color=:red)
                printstyled(io, " help: ", color=:blue)
                print(io, "The parser expected a macroname after this backtick, but instead found ")
                if iskeyword(theerror.got)
                    printstyled(io, "a keyword", bold=true)
                elseif theerror.got === RESERVED
                    printstyled(io, "a reserved word", bold=true)
                else
                    print(io, "something else")
                end
                println(io, ".")
                printstyled(io, "│", color=:red)
                print(io, "       ", "Check for spelling mistakes.")
                println(io)
                printstyled(io, "╯", color=:red)
                println(io)
                return false
            elseif theerror.kind == RecursiveMacro
                printstyled(io, "╭ ERROR [VAS04]:", bold=true, color=:red)
                printstyled(io, " Macro Evaluation recursed.", color=:red)
                println(io)
                mname = String(child)
                annotations = [
                    PipeAnnotation(spanvirtrange(child), Cell(fg = :light_red), String("✗ Recusive macro expansion"));
                    HighlightAnnotation(spanvirtrange(child), Cell(fg = :light_red))]
                print_snippet(io, va.ps, contextrange, annotations)
                print(io, "Macro Evaluation recursed ")
                printstyled(io, mname, color=:default, bold=true)
                println(io, ".")
                printstyled(io, "│", color=:red)
                println(io)
                printstyled(io, "│", color=:red)
                printstyled(io, " help: ", color=:blue)
                print(io, "The code made use of the macro ")
                printstyled(io, mname, color=:default, bold=true)
                print(io, " while evaluation the same macro.")
                println(io)
                printstyled(io, "│", color=:red)
                print(io, "       ", "Check for spelling mistakes or definition ordering.")
                println(io)
                printstyled(io, "╯", color=:red)
                println(io)
                return false
            elseif theerror.kind == MissingAssignment
                printstyled(io, "╭ ERROR [VAS14]:", bold=true, color=:red)
                printstyled(io, " Statements without assignments are illegal in Verilog-A.", color=:red)
                println(io)

                annotations = Any[]

                nn = AbstractTrees.nextsibling(tc)
                nn′ = nodevalue(AbstractTrees.nextsibling(nn))

                if isa(nn′, Node) && !isa(nn′, Node{Error})
                    if isa(nn′, Node{FunctionCall})
                        push!(annotations, CaretAnnotation(first(spanvirtrange(nn′))-1, "◌ ", "dummy = ", false))
                    end
                    push!(annotations, PipeAnnotation(spanvirtrange(nn′), Cell(fg = :light_red), String("Statement without assignment")))
                    contextrange = first(contextrange):max(last(spanvirtrange(nn′)), last(contextrange))
                end

                print_snippet(io, va.ps, contextrange, annotations)
                return false
            else
                @show child.expr.kind
            end

            print_snippet(io, va.ps, contextrange, [annotation])

            println(io)
            return false
        end
        if isempty(children(child)) && child !== nothing
            prevleaf=child
        end
    end
    return true
end
