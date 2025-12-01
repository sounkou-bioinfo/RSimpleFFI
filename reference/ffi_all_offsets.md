# Get all field offsets for a struct

Returns a named integer vector with byte offsets for all fields.

## Usage

``` r
ffi_all_offsets(struct_type)
```

## Arguments

- struct_type:

  StructType object

## Value

Named integer vector of offsets

## Examples

``` r
if (FALSE) { # \dontrun{
Point <- ffi_struct(x = ffi_int(), y = ffi_double())
ffi_all_offsets(Point)
# x y
# 0 8
} # }
```
