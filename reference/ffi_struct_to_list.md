# Convert struct to R list

Convert struct to R list

## Usage

``` r
ffi_struct_to_list(ptr, struct_type)
```

## Arguments

- ptr:

  External pointer to struct

- struct_type:

  StructType object

## Value

Named list of field values

## Examples

``` r
if (FALSE) { # \dontrun{
Point <- ffi_struct(x = ffi_int(), y = ffi_int())
pt <- ffi_alloc(Point)
ffi_set_field(pt, "x", 42L, Point)
ffi_set_field(pt, "y", 100L, Point)
as.list(pt, Point) # list(x = 42L, y = 100L)
} # }
```
