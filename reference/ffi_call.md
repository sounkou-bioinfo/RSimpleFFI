# Make FFI function call

Call a C function through the FFI interface.

## Usage

``` r
ffi_call(cif, symbol, ...)
```

## Arguments

- cif:

  CIF object defining the call interface

- symbol:

  NativeSymbol or character name of function

- ...:

  Arguments to pass to the function (including `na_check`)

## Value

The return value from the C function, converted to an R type

## Details

The method implementations accept an additional `na_check` argument
(logical, default TRUE). When TRUE, the function checks for NA values in
arguments and errors if found. Set to FALSE to skip NA checking for
better performance (at your own risk).

### Error Handling Limitations

**Important:** libffi provides no error handling for the actual C
function call. If the called C function crashes (segmentation fault,
illegal instruction, abort, etc.), R itself will crash. This is a
fundamental limitation of FFI

- there is no portable way to catch such errors in C code.

Before making FFI calls, ensure:

- The function pointer is valid (not NULL, points to executable code)

- All pointer arguments are valid (use
  [`ffi_is_null`](https://sounkou-bioinfo.github.io/RSimpleFFI/reference/ffi_is_null.md)
  to check)

- Array/buffer sizes are correct - buffer overruns cause undefined
  behavior

- The CIF signature exactly matches the C function's signature

- Struct layouts match between R types and C (check alignment/padding)

For debugging crashes:

- Run R under a debugger: `R -d gdb`

- Enable core dumps: `ulimit -c unlimited`

- Use address sanitizers when building the library being called

## See also

[`ffi_is_null`](https://sounkou-bioinfo.github.io/RSimpleFFI/reference/ffi_is_null.md)
for checking pointer validity
