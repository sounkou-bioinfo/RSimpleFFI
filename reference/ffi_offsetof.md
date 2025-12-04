# Get byte offset of a field in a structure

Returns the byte offset of a field within a structure, accounting for
alignment requirements. Similar to C's offsetof() macro.

## Usage

``` r
ffi_offsetof(struct_type, field, use_pack = TRUE)
```

## Arguments

- struct_type:

  StructType object

- field:

  Character field name or integer field index (1-based)

- use_pack:

  Logical, whether to use the struct's pack setting. Default TRUE. Set
  to FALSE to get libffi's natural alignment offset even for packed
  structs.

## Value

Integer byte offset

## Details

For packed structures (created with `pack` parameter), this function
computes the offset using the specified packing alignment rather than
natural alignment.

## Examples

``` r
if (FALSE) { # \dontrun{
Point <- ffi_struct(x = ffi_int(), y = ffi_double())
ffi_offsetof(Point, "x") # 0
ffi_offsetof(Point, "y") # 8 (aligned to 8-byte boundary)

# Packed struct example
Packed <- ffi_struct(a = ffi_uint8(), b = ffi_int32(), .pack = 1)
ffi_offsetof(Packed, "b") # 1 (no padding with .pack=1)
} # }
```
