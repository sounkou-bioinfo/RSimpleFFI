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
