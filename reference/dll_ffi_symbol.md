# Create FFI function from dynamically loaded function

This creates an FFI function wrapper for dynamically loaded native C
functions. Uses direct address access like Rffi for maximum
compatibility.

## Usage

``` r
dll_ffi_symbol(symbol_name, return_type, ..., package = NULL, na_check = TRUE)
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

- na_check:

  Logical; if TRUE (default), check for NA values and error if found.
  Set to FALSE to skip NA checking for better performance (at your own
  risk).

## Value

FFI function object that can be called directly
