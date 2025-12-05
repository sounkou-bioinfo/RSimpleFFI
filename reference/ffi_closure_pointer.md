# Get the function pointer for an FFI closure

Returns the executable function pointer that can be passed to C
functions expecting a callback.

## Usage

``` r
ffi_closure_pointer(closure)
```

## Arguments

- closure:

  An FFIClosure object

## Value

External pointer to the callable function

## Details

The returned pointer can be passed to C functions via
[`ffi_call()`](https://sounkou-bioinfo.github.io/RSimpleFFI/reference/ffi_call.md).
It will invoke the R function when called.

## See also

[ffi_closure()](https://sounkou-bioinfo.github.io/RSimpleFFI/reference/ffi_closure.md)
to create closures
