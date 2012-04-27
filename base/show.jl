print(x) = fprint(stdout_stream, x)
show(x) = fshow(stdout_stream, x)
fprint(io, x) = fshow(io, x)

# formerly built-in methods. can be replaced any time.
fprint(io, a::Array{Uint8,1}) =
    ccall(:jl_print_array_uint8, Void, (Ptr{Void}, Any,), io, a)
fprint(io, s::Symbol) = ccall(:jl_print_symbol, Void, (Ptr{Void}, Any,), io, s)
fshow(io, x) = ccall(:jl_fshow_any, Void, (Ptr{Void}, Any,), io, x)

fshowcompact(io, x) = fshow(io, x)
showcompact(x) = fshowcompact(stdout_stream, x)

fshow(io, s::Symbol) = fprint(io, s)
fshow(io, tn::TypeName) = fshow(io, tn.name)
fshow(io, ::Nothing) = fprint(io, "nothing")
fshow(io, b::Bool) = fprint(io, b ? "true" : "false")
fshow(io, n::Integer)  = fprint(io, dec(int64(n)))

function fshow_trailing_hex(io, n::Uint64, ndig::Integer)
    for s = ndig-1:-1:0
        d = (n >> 4*s) & uint64(0xf)
        fprint(io, "0123456789abcdef"[d+1])
    end
end
fshow(io, n::Unsigned) = (fprint(io, "0x");
                       fshow_trailing_hex(io, uint64(n), sizeof(n)<<1))

show{T}(io, p::Ptr{T}) =
    fprint(io, is(T,None) ? "Ptr{Void}" : typeof(p), " @0x$(hex(unsigned(p), WORD_SIZE>>2))")

function fshow(io, l::LambdaStaticData)
    fprint(io, "AST(")
    fshow(io, l.ast)
    fprint(io, ")")
end

function fshow_delim_array(io, itr, open, delim, close, delim_one)
    fprint(io, open)
    state = start(itr)
    newline = true
    first = true
    if !done(itr,state)
	while true
	    x, state = next(itr,state)
            multiline = isa(x,AbstractArray) && ndims(x)>1 && numel(x)>0
            if newline
                if multiline; fprintln(io); end
            end
	    fshow(io, x)
	    if done(itr,state)
                if delim_one && first
                    fprint(io, delim)
                end
		break
	    end
            first = false
            fprint(io, delim)
            if multiline
                fprintln(io); fprintln(io); newline=false
            else
                newline = true
            end
	end
    end
    fprint(io, close)
end

fshow_comma_array(io, itr, o, c) = fshow_delim_array(io, itr, o, ',', c, false)
fshow(io, t::Tuple) = fshow_delim_array(io, t, '(', ',', ')', true)

function fshow_expr_type(io, ty)
    if !is(ty, Any)
        if is(ty, Function)
            fprint(io, "::F")
        elseif is(ty, IntrinsicFunction)
            fprint(io, "::I")
        else
            fprint(io, "::$ty")
        end
    end
end

function fshow(io, e::Expr)
    hd = e.head
    if is(hd,:call)
        fprint(io, e.args[1])
        fshow_comma_array(io, e.args[2:],'(',')')
    elseif is(hd,:(=))
        fprint(io, "$(e.args[1]) = $(e.args[2])")
    elseif is(hd,:null)
        fprint(io, "nothing")
    elseif is(hd,:gotoifnot)
        fprint(io, "unless $(e.args[1]) goto $(e.args[2])")
    elseif is(hd,:return)
        fprint(io, "return $(e.args[1])")
    elseif is(hd,:string)
        fshow(io, e.args[1])
    elseif is(hd,symbol("::"))
        fshow(io, e.args[1])
        fprint(io, "::")
        fshow(io, e.args[2])
    elseif is(hd,:quote)
        fshow_quoted_expr(io, e.args[1])
    elseif is(hd,:body) || is(hd,:block)
        fprintln(io, "\nbegin")
        for a in e.args
            fprint(io, "  ")
            fshow(io, a)
            fprintln(io)
        end
        fprintln(io, "end")
    elseif is(hd,:comparison)
        for a in e.args
            fshow(io, a)
        end
    elseif is(hd,:(.))
        fshow(io, e.args[1])
        fprint(io, '.')
        fshow(io, e.args[2])
    else
        fprint(io, hd)
        fshow_comma_array(io, e.args,'(',')')
    end
    fshow_expr_type(io, e.typ)
end

fshow(io, e::SymbolNode) = (fprint(io, e.name); fshow_expr_type(io, e.typ))
fshow(io, e::LineNumberNode) = fprint(io, "line($(e.line))")
fshow(io, e::LabelNode) = fprint(io, "$(e.label): ")
fshow(io, e::GotoNode) = fprint(io, "goto $(e.label)")
fshow(io, e::TopNode) = fprint(io, "top($(e.name))")
fshow(io, e::QuoteNode) = fshow_quoted_expr(io, e.value)

function fshow_quoted_expr(io, a1)
    if isa(a1,Expr) && (is(a1.head,:body) || is(a1.head,:block))
        fprintln(io, "\nquote")
        for a in a1.args
            fprint(io, "  ")
            fshow(io, a)
            fprintln(io, )
        end
        fprintln(io, "end")
    else
        if isa(a1,Symbol) && !is(a1,:(:)) && !is(a1,:(==))
            fprint(io, ":$a1")
        else
            fprint(io, ":($a1)")
        end
    end
end

function fshow(io, e::TypeError)
    ctx = isempty(e.context) ? "" : "in $(e.context), "
    if e.expected == Bool
        fprint(io, "type error: non-boolean ($(typeof(e.got))) ",
                   "used in boolean context")
    else
        if isa(e.got,Type)
            tstr = "Type{$(e.got)}"
        else
            tstr = string(typeof(e.got))
        end
        fprint(io, "type error: $(e.func): ",
                   "$(ctx)expected $(e.expected), ",
                   "got $tstr")
    end
end

fshow(io, e::LoadError) = (fshow(io, e.error); fprint(io, "\nat $(e.file):$(e.line)"))
fshow(io, e::SystemError) = fprint(io, "$(e.prefix): $(strerror(e.errnum))")
fshow(io, ::DivideByZeroError) = fprint(io, "error: integer divide by zero")
fshow(io, ::StackOverflowError) = fprint(io, "error: stack overflow")
fshow(io, ::UndefRefError) = fprint(io, "access to undefined reference")
fshow(io, ::EOFError) = fprint(io, "read: end of file")
fshow(io, e::ErrorException) = fprint(io, e.msg)
fshow(io, e::KeyError) = fprint(io, "key not found: $(e.key)")
fshow(io, e::InterruptException) = nothing

function fshow(io, e::MethodError)
    name = e.f.env.name
    if is(e.f,convert)
        fprint(io, "no method $(name)(Type{$(e.args[1])},$(typeof(e.args[2])))")
    else
        fprint(io, "no method $(name)$(typeof(e.args))")
    end
end

function fshow(io, bt::BackTrace)
    fshow(io, bt.e)
    t = bt.trace
    # we may not declare :_jl_eval_user_input
    # directly so that we get a compile error
    # in case its name changes in the future
    const _jl_eval_function = symbol(string(_jl_eval_user_input))
    for i = 1:3:length(t)
        if i == 1 && t[i] == :error; continue; end
        if t[i] == _jl_eval_function; break; end
        fprint(io, "\n")
        lno = t[i+2]
        fprint(io, " in ", t[i], " at ", t[i+1])
        if lno >= 1
            fprint(io, ":", lno)
        end
    end
end

function dump(x)
    T = typeof(x)
    if isa(x,Array)
        fprint(io, "Array($(eltype(x)),$(size(x)))")
    elseif isa(T,CompositeKind)
        fprint(io, T,'(')
        for field = T.names
            fprint(io, field, '=')
            dump(getfield(x, field))
            fprint(io, ',')
        end
        fprintln(io, ')')
    else
        fshow(io, x)
    end
end

function showall{T}(a::AbstractArray{T,1})
    if is(T,Any)
        opn = '{'; cls = '}'
    else
        opn = '['; cls = ']';
    end
    fshow_comma_array(io, a, opn, cls)
end

alignment(x::Any) = (0, strlen(sprint(showcompact, x)))
alignment(x::Number) = (strlen(sprint(showcompact, x)), 0)
alignment(x::Integer) = (strlen(sprint(showcompact, x)), 0)
function alignment(x::Real)
    m = match(r"^(.*?)((?:[\.eE].*)?)$", sprint(showcompact, x))
    m == nothing ? (strlen(sprint(showcompact, x)), 0) :
                   (strlen(m.captures[1]), strlen(m.captures[2]))
end
function alignment(x::Complex)
    m = match(r"^(.*,)(.*)$", sprint(showcompact, x))
    m == nothing ? (strlen(sprint(showcompact, x)), 0) :
                   (strlen(m.captures[1]), strlen(m.captures[2]))
end
function alignment(x::Rational)
    m = match(r"^(.*?/)(/.*)$", sprint(showcompact, x))
    m == nothing ? (strlen(sprint(showcompact, x)), 0) :
                   (strlen(m.captures[1]), strlen(m.captures[2]))
end

const _jl_undef_ref_str = "#undef"
const _jl_undef_ref_alignment = (3,3)

function alignment(
    X::AbstractMatrix,
    rows::AbstractVector, cols::AbstractVector,
    cols_if_complete::Integer, cols_otherwise::Integer, sep::Integer
)
    a = {}
    for j in cols
        l = r = 0
        for i in rows
            aij = _jl_undef_ref_alignment
            try
                aij = alignment(X[i,j])
            end
            l = max(l, aij[1])
            r = max(r, aij[2])
        end
        push(a, (l, r))
        if sum(map(sum,a)) + sep*length(a) >= cols_if_complete
            pop(a)
            break
        end
    end
    if length(a) < size(X,2)
        while sum(map(sum,a)) + sep*length(a) >= cols_otherwise
            pop(a)
        end
    end
    return a
end

function fprint_matrix_row(io,
    X::AbstractMatrix, A::Vector,
    i::Integer, cols::AbstractVector, sep::String
)
    for k = 1:length(A)
        j = cols[k]
        a = _jl_undef_ref_alignment
        sx = _jl_undef_ref_str
        try
            x = X[i,j]
            a = alignment(x)
            sx = sprint(showcompact, x)
        end
        l = repeat(" ", A[k][1]-a[1])
        r = repeat(" ", A[k][2]-a[2])
        fprint(io, l, sx, r)
        if k < length(A); fprint(io, sep); end
    end
end

function fprint_matrix_vdots(io,
    vdots::String, A::Vector, sep::String, M::Integer, m::Integer
)
    for k = 1:length(A)
        w = A[k][1] + A[k][2]
        if k % M == m
            l = repeat(" ", max(0, A[k][1]-strlen(vdots)))
            r = repeat(" ", max(0, w-strlen(vdots)-strlen(l)))
            fprint(io, l, vdots, r)
        else
            fprint(io, repeat(" ", w))
        end
        if k < length(A); fprint(io, sep); end
    end
end

function fprint_matrix(io, 
    X::AbstractMatrix, rows::Integer, cols::Integer,
    pre::String, sep::String, post::String,
    hdots::String, vdots::String,
    hmod::Integer, vmod::Integer
)
    cols -= strlen(pre) + strlen(post)
    presp = repeat(" ", strlen(pre))
    postsp = ""
    hdotssp = repeat(" ", strlen(hdots))
    ss = strlen(sep)
    m, n = size(X)
    if m <= rows # rows fit
        A = alignment(X,1:m,1:n,cols,cols,ss)
        if n <= length(A) # rows and cols fit
            for i = 1:m
                fprint(io, i == 1 ? pre : presp)
                fprint_matrix_row(io, X,A,i,1:n,sep)
                fprint(io, i == m ? post : postsp)
                if i != m; fprintln(io, ); end
            end
        else # rows fit, cols don't
            c = div(cols-strlen(hdots)+1,2)+1
            R = reverse(alignment(X,1:m,n:-1:1,c,c,ss))
            c = cols - sum(map(sum,R)) - (length(R)-1)*ss - strlen(hdots)
            L = alignment(X,1:m,1:n,c,c,ss)
            for i = 1:m
                fprint(io, i == 1 ? pre : presp)
                fprint_matrix_row(io, X,L,i,1:length(L),sep)
                fprint(io, i % hmod == 1 ? hdots : repeat(" ", strlen(hdots)))
                fprint_matrix_row(io, X,R,i,n-length(R)+1:n,sep)
                fprint(io, i == m ? post : postsp)
                if i != m; fprintln(io, ); end
            end
        end
    else # rows don't fit
        t = div(rows,2)
        I = [1:t; m-div(rows-1,2)+1:m]
        A = alignment(X,I,1:n,cols,cols,ss)
        if n <= length(A) # rows don't fit, cols do
            for i in I
                fprint(io, i == 1 ? pre : presp)
                fprint_matrix_row(io, X,A,i,1:n,sep)
                fprint(io, i == m ? post : postsp)
                if i != I[end]; fprintln(io, ); end
                if i == t
                    fprint(io, i == 1 ? pre : presp)
                    fprint_matrix_vdots(io, vdots,A,sep,vmod,1)
                    fprintln(io, i == m ? post : postsp)
                end
            end
        else # neither rows nor cols fit
            c = div(cols-strlen(hdots)+1,2)+1
            R = reverse(alignment(X,I,n:-1:1,c,c,ss))
            c = cols - sum(map(sum,R)) - (length(R)-1)*ss - strlen(hdots)
            L = alignment(X,I,1:n,c,c,ss)
            r = (length(R)-n+1) % vmod
            for i in I
                fprint(io, i == 1 ? pre : presp)
                fprint_matrix_row(io, X,L,i,1:length(L),sep)
                fprint(io, i % hmod == 1 ? hdots : repeat(" ", strlen(hdots)))
                fprint_matrix_row(io, X,R,i,n-length(R)+1:n,sep)
                fprint(io, i == m ? post : postsp)
                if i != I[end]; fprintln(io, ); end
                if i == t
                    fprint(io, i == 1 ? pre : presp)
                    fprint_matrix_vdots(io, vdots,L,sep,vmod,1)
                    fprint(io, hdotssp)
                    fprint_matrix_vdots(io, vdots,R,sep,vmod,r)
                    fprintln(io, i == m ? post : postsp)
                end
            end
        end
    end
end
fprint_matrix(io, X::AbstractMatrix, rows::Integer, cols::Integer) =
    fprint_matrix(io, X, rows, cols, " ", "  ", "", "  :  ", ":", 5, 5)

fprint_matrix(io, X::AbstractMatrix) = fprint_matrix(io, X, tty_rows()-4, tty_cols())

summary(x) = string(typeof(x))

dims2string(d) = length(d) == 0 ? "0-dimensional" :
                 length(d) == 1 ? "$(d[1])-element" :
                 join(map(string,d), 'x')

summary{T}(a::AbstractArray{T}) =
    strcat(dims2string(size(a)), " ", string(T), " ", string(typeof(a).name))

function fshow_nd(io, a::AbstractArray)
    if isempty(a)
        return
    end
    tail = size(a)[3:]
    nd = ndims(a)-2
    function fprint_slice(io, idxs...)
        for i = 1:nd
            ii = idxs[i]
            if size(a,i+2) > 10
                if ii == 4 && allp(x->x==1,idxs[1:i-1])
                    for j=i+1:nd
                        szj = size(a,j+2)
                        if szj>10 && 3 < idxs[j] <= szj-3
                            return
                        end
                    end
                    #fprintln(io, idxs)
                    fprint(io, "...\n\n")
                    return
                end
                if 3 < ii <= size(a,i+2)-3
                    return
                end
            end
        end
        fprint(io, "[:, :, ")
        for i = 1:(nd-1); fprint(io, "$(idxs[i]), "); end
        fprintln(io, idxs[end], "] =")
        slice = a[:,:,idxs...]
        fprint_matrix(io, reshape(slice, size(slice,1), size(slice,2)))
        fprint(io, idxs == tail ? "" : "\n\n")
    end
    cartesian_map(print_slice, tail)
end

function whos()
    global VARIABLES
    for v = map(symbol,sort(map(string, VARIABLES)))
        if isbound(v)
            fprintln(io, rpad(v, 30), summary(eval(v)))
        end
    end
end

show{T}(x::AbstractArray{T,0}) = (fprintln(io, summary(x),":"); fshow(io, x[]))
function fshow(io, X::AbstractArray)
    fprint(io, summary(X))
    if !isempty(X)
        fprintln(io, ":")
        ndims(X)==2 ? fprint_matrix(io, X) : fshow_nd(io, X)
    end
end

function fshowall(io, X::AbstractMatrix)
    fprint(io, summary(X))
    if !isempty(X)
        fprintln(io, ":")
        fprint_matrix(io, X, typemax(Int64), typemax(Int64))
    end
end

function fshowall(io, a::AbstractArray)
    fprint(io, summary(a))
    if isempty(a)
        return
    end
    fprintln(io, ":")
    tail = size(a)[3:]
    nd = ndims(a)-2
    function fprint_slice(io, idxs...)
        fprint(io, "[:, :, ")
        for i = 1:(nd-1); fprint(io, "$(idxs[i]), "); end
        fprintln(io, idxs[end], "] =")
        slice = a[:,:,idxs...]
        fprint_matrix(io, reshape(slice, size(slice,1), size(slice,2)),
                     typemax(Int64), typemax(Int64))
        fprint(io, idxs == tail ? "" : "\n\n")
    end
    cartesian_map(print_slice, tail)
end

function fshow_vector(io, v, opn, cls)
    X = reshape(v,(1,length(v)))
    fprint_matrix(io, X, 1, tty_cols(), opn, ", ", cls, "  ...  ", ":", 5, 5)
end

fshow(io, v::AbstractVector{Any}) = fshow_vector(io, v, "{", "}")
fshow(io, v::AbstractVector)      = fshow_vector(io, v, "[", "]")
