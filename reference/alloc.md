# Allocate a buffer for a given FFI type

Allocates a buffer for n elements of the given FFIType (e.g., int,
double, etc). Returns an external pointer tagged with the type.

## Usage

``` r
ffi_alloc(type, ...)
```

## Arguments

- type:

  FFIType object

- ...:

  Additional arguments

## Value

External pointer to buffer
