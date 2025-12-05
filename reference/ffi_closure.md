# Create an FFI closure from an R function

Wraps an R function so it can be used as a callback from C code. The
closure has a CIF that describes its signature (return type and argument
types). When C code calls through the closure's function pointer, the R
function is invoked with converted arguments.

## Usage

``` r
ffi_closure(r_function, return_type, ...)
```

## Arguments

- r_function:

  An R function to wrap as a callback

- return_type:

  FFIType for return value

- ...:

  FFIType objects for arguments

## Value

An FFIClosure object

## Details

The R function must accept the same number of arguments as specified in
the type signature. Arguments are converted from C types to R types
before calling, and the return value is converted back to C.

Important: You must keep a reference to the FFIClosure object for as
long as C code might call through it. If the closure is garbage
collected, calling through its function pointer will crash.

## See also

[ffi_closure_pointer()](https://sounkou-bioinfo.github.io/RSimpleFFI/reference/ffi_closure_pointer.md)
to get the callable function pointer

## Examples

``` r
if (FALSE) { # \dontrun{
# Create a comparison function for qsort
cmp_fn <- function(a, b) {
  as.integer(a - b)
}

# Wrap it as a C callback: int (*)(int*, int*)
cmp_closure <- ffi_closure(
  cmp_fn,
  ffi_int(), # return type
  ffi_pointer(), ffi_pointer() # argument types (pointers to int)
)

# Get the function pointer to pass to C
cmp_ptr <- ffi_closure_pointer(cmp_closure)
} # }
```
