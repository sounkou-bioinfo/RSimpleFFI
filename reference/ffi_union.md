# Create FFI union type

Creates a union type where all fields share the same memory location.
The union's size is the size of its largest member.

## Usage

``` r
ffi_union(..., pack = NULL)
```

## Arguments

- ...:

  Named FFIType objects representing union fields

- pack:

  Integer packing alignment (1, 2, 4, 8, or 16). When specified, the
  union's alignment is reduced to min(natural_alignment, pack). This
  affects placement when the union is used as a struct member. Default
  NULL uses natural alignment.

## Value

UnionType object

## Examples

``` r
# Normal union
U <- ffi_union(c = ffi_char(), i = ffi_int())

# Packed union (alignment = 1)
PackedU <- ffi_union(c = ffi_char(), i = ffi_int(), pack = 1)

# Packed union in a struct - offset of next field is affected
S <- ffi_struct(u = PackedU, after = ffi_char())
```
