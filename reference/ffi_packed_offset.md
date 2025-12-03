# Compute packed offset for a field

Computes the byte offset of a field using packed alignment rules. This
is used internally when a struct has a pack parameter set. Uses C
implementation for accurate compiler-matching behavior.

## Usage

``` r
ffi_packed_offset(struct_type, field_index)
```

## Arguments

- struct_type:

  StructType object with pack property set

- field_index:

  Integer field index (1-based)

## Value

Integer byte offset
