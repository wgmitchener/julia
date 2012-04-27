module Base

if false
    # simple print definitions for debugging. enable these if something
    # goes wrong during bootstrap before printing code is available.
    length(a::Array) = arraylen(a)
    fprint(io, a::Array{Uint8,1}) = ccall(:jl_print_array_uint8, Void, (Any,), a)
    fprint(io, s::Symbol) = ccall(:jl_print_symbol, Void, (Any,), s)
    fprint(io, s::ASCIIString) = fprint(io, s.data)
    fprint(io, x) = fshow(io, x)
    fprintln(io, x) = (fprint(io, x); fprint(io, "\n"))
    fshow(io, x) = ccall(:jl_fshow_any, Void, (Ptr{Void}, Any,), io, x)
    fshow(io, s::ASCIIString) = fprint(io, s.data)
    fshow(io, s::Symbol) = fprint(io, s)
    fshow(io, b::Bool) = fprint(io, b ? "true" : "false")
    fshow(io, n::Int64) = ccall(:jl_print_int64, Void, (Ptr{Void}, Int64,), io, n)
    fshow(io, n::Integer)  = fshow(io, int64(n))
    fprint(io, a...) = for x=a; fprint(io, x); end
    function fshow(io, e::Expr)
        fprint(io, e.head)
        fprint(io, "(")
        for i=1:arraylen(e.args)
            fshow(io, arrayref(e.args,i))
            fprint(io, ", ")
        end
        fprint(io, ")\n")
    end
end

include("sysimg.jl")

ccall(:jl_save_system_image, Void, (Ptr{Uint8},Ptr{Uint8}),
      "$JULIA_HOME/sys0.ji", "start_image.jl")

end # module
