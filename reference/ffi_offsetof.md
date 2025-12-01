# Get byte offset of a field in a structure

Returns the byte offset of a field within a structure, accounting for
alignment requirements. Similar to C's offsetof() macro.

## Usage

``` r
ffi_offsetof(struct_type, field)
```

## Arguments

- struct_type:

  StructType object

- field:

  Character field name or integer field index (1-based)

## Value

Integer byte offset

## Examples

``` r
if (FALSE) { # \dontrun{
Point <- ffi_struct(x = ffi_int(), y = ffi_double())
ffi_offsetof(Point, "x") # 0
ffi_offsetof(Point, "y") # 8 (aligned to 8-byte boundary)
} # }
```
