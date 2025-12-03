# Compute packed size for a struct

Computes the total size of a struct using packed alignment rules. The
size includes any trailing padding needed for array alignment. Uses C
implementation for accurate compiler-matching behavior.

## Usage

``` r
ffi_packed_size(struct_type)
```

## Arguments

- struct_type:

  StructType object with pack property set

## Value

Integer size in bytes
