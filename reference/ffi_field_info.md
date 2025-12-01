# Get field information from a struct type

Returns metadata about a specific field, including its byte offset,
size, and type information.

## Usage

``` r
ffi_field_info(struct_type, field)
```

## Arguments

- struct_type:

  StructType object

- field:

  Character field name or integer field index (1-based)

## Value

FieldInfo object with offset, size, alignment info

## Examples

``` r
if (FALSE) { # \dontrun{
Point <- ffi_struct(x = ffi_int(), y = ffi_double())
ffi_field_info(Point, "x")
# <FieldInfo 'x' type=int, offset=0, size=4>
ffi_field_info(Point, "y")
# <FieldInfo 'y' type=double, offset=8, size=8>  (offset 8 due to alignment)
} # }
```
