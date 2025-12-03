# Unpack bit-fields from a 64-bit value

Extracts multiple values from a packed 64-bit value according to
specified bit widths.

## Usage

``` r
ffi_unpack_bits64(packed_value, widths)
```

## Arguments

- packed_value:

  Packed value (as double for 64-bit range)

- widths:

  Integer vector of bit widths for each field

## Value

Integer vector of unpacked values

## Examples

``` r
packed <- ffi_pack_bits64(c(1L, 5L, 12L), c(1L, 3L, 4L))
ffi_unpack_bits64(packed, c(1L, 3L, 4L))  # c(1, 5, 12)
#> [1]  1  5 12
```
