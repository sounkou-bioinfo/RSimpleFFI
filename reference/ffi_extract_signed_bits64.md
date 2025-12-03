# Extract a signed bit-field from a 64-bit packed value

Extracts a bit-field and sign-extends it based on the high bit. Useful
for signed integer fields in C structures.

## Usage

``` r
ffi_extract_signed_bits64(packed_value, bit_offset, bit_width)
```

## Arguments

- packed_value:

  Packed value (as double for 64-bit range)

- bit_offset:

  Bit offset from LSB (0-based)

- bit_width:

  Number of bits in the field

## Value

Extracted signed value as double

## Examples

``` r
# Pack a negative value in 4-bit signed field (-3 = 0xD in 4 bits)
packed <- ffi_pack_bits64(c(13L), c(4L)) # 0xD = -3 as signed 4-bit
ffi_extract_signed_bits64(packed, 0L, 4L) # -3
#> [1] -3
```
