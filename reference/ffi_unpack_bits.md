# Unpack bit-fields from an integer

Extracts multiple values from a packed integer according to specified
bit widths. This is the inverse operation of `ffi_pack_bits`.

## Usage

``` r
ffi_unpack_bits(packed_value, widths)
```

## Arguments

- packed_value:

  Integer value containing packed bit-fields

- widths:

  Integer vector of bit widths for each field

## Value

Integer vector of unpacked values

## Details

Values are unpacked from LSB to MSB (least significant bit to most
significant bit). Each value is extracted by shifting and masking
according to its width.

## Examples

``` r
# Unpack a value with three bit-fields
values <- ffi_unpack_bits(0x65L, c(1L, 3L, 4L))
values # [1]  1  5 12
#> [1] 1 2 6

# Round-trip test
packed <- ffi_pack_bits(c(1L, 5L, 12L), c(1L, 3L, 4L))
identical(ffi_unpack_bits(packed, c(1L, 3L, 4L)), c(1L, 5L, 12L))
#> [1] TRUE
```
