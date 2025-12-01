# Read a global variable from a shared library

Reads a typed value from a global symbol address. This is a typed
version of ffi_deref_pointer that handles type conversion.

## Usage

``` r
ffi_read_global(ptr, type)
```

## Arguments

- ptr:

  External pointer to the global variable address

- type:

  FFIType describing the type of the global variable

## Value

The value at the address, converted to an appropriate R type
