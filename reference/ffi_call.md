# Make FFI function call

Make FFI function call

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

  Arguments to pass to the function

- na_check:

  Logical; if TRUE (default), check for NA values and error if found.
  Set to FALSE to skip NA checking for better performance (at your own
  risk).
