# Get element from struct array

Returns a pointer to the i-th struct in a contiguous array of structs.
The returned pointer shares memory with the original array.

## Usage

``` r
ffi_get_element(ptr, index, struct_type)
```

## Arguments

- ptr:

  External pointer to struct array (from ffi_alloc with n \> 1)

- index:

  1-based index of element to get

- struct_type:

  StructType describing the element type

## Value

External pointer to the element (no finalizer - parent owns memory)

## Examples

``` r
if (FALSE) { # \dontrun{
Point <- ffi_struct(x = ffi_int(), y = ffi_int())
points <- ffi_alloc(Point, 10L)  # array of 10 Points
p3 <- ffi_get_element(points, 3L, Point)
ffi_set_field(p3, "x", 100L, Point)
} # }
```
