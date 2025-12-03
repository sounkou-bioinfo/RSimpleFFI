# Create and initialize a struct from R list

Create and initialize a struct from R list

## Usage

``` r
ffi_struct_from_list(struct_type, values)
```

## Arguments

- struct_type:

  StructType object

- values:

  Named list of field values

## Value

External pointer to allocated and initialized struct

## Examples

``` r
if (FALSE) { # \dontrun{
Point <- ffi_struct(x = ffi_int(), y = ffi_int())
pt <- ffi_struct_from_list(Point, list(x = 10L, y = 20L))
} # }
```
