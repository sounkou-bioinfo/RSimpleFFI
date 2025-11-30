# Allocate a raw memory buffer (external pointer, auto-finalized)

Allocates a buffer of the given size (in bytes) and returns an external
pointer. The memory is automatically freed when the pointer is garbage
collected.

## Usage

``` r
ffi_alloc_buffer(size)
```

## Arguments

- size:

  Number of bytes to allocate

## Value

External pointer to buffer
