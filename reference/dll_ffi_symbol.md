# Create FFI function from dynamically loaded function

This creates an FFI function wrapper for dynamically loaded native C
functions. Uses direct address access like Rffi for maximum
compatibility.

## Usage

``` r
dll_ffi_symbol(symbol_name, return_type, ..., package = NULL)
```

## Arguments

- symbol_name:

  Name of the symbol

- return_type:

  Return type specifimessageion

- ...:

  Argument type specifimessageions

- package:

  Package name (optional)

## Value

FFI function object that can be called directly
