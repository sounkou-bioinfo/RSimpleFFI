# Allocate array of structs from R list

Allocate array of structs from R list

## Usage

``` r
ffi_struct_array_from_list(struct_type, values)
```

## Arguments

- struct_type:

  StructType object

- values:

  List of named lists, one per struct

## Value

External pointer to allocated struct array

## Examples

``` r
if (FALSE) { # \dontrun{
Point <- ffi_struct(x = ffi_int(), y = ffi_int())
points <- ffi_struct_array_from_list(Point, list(
  list(x = 0L, y = 0L),
  list(x = 10L, y = 20L),
  list(x = 30L, y = 40L)
))
} # }
```
