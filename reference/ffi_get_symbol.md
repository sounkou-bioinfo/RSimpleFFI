# Get function pointer from compiled library

Extract a function pointer from a compiled library for use with .Call().

## Usage

``` r
ffi_get_symbol(lib, symbol_name)
```

## Arguments

- lib:

  Compiled library from ffi_compile_shlib()

- symbol_name:

  Name of the C function

## Value

Function that calls the C function via .Call()
