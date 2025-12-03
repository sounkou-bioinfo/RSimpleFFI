# Extract a signed bit-field from a packed value

Extracts a single bit-field and sign-extends it based on the high bit.
This is the 32-bit version using pure R code.

## Usage

``` r
ffi_extract_signed_bit_field(packed_value, bit_offset, bit_width)
```

## Arguments

- packed_value:

  Integer value containing packed bit-fields

- bit_offset:

  Bit offset from LSB (0-based)

- bit_width:

  Number of bits in the field

## Value

Extracted signed integer value

## Examples

``` r
# A 4-bit value of 13 (0xD) represents -3 in signed 4-bit
packed <- ffi_pack_bits(c(13L), c(4L))
ffi_extract_signed_bit_field(packed, 0L, 4L)  # -3
#> [1] -3

# 3-bit value of 7 represents -1 in signed 3-bit
packed2 <- ffi_pack_bits(c(7L), c(3L))
ffi_extract_signed_bit_field(packed2, 0L, 3L)  # -1
#> [1] -1
```
