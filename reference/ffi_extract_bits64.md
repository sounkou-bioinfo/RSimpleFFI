# Extract a single bit-field from a 64-bit packed value

Extract a single bit-field from a 64-bit packed value

## Usage

``` r
ffi_extract_bits64(packed_value, bit_offset, bit_width)
```

## Arguments

- packed_value:

  Packed value (as double for 64-bit range)

- bit_offset:

  Bit offset from LSB (0-based)

- bit_width:

  Number of bits in the field

## Value

Extracted value as double (for 64-bit range)

## Examples

``` r
packed <- ffi_pack_bits64(c(1L, 5L, 12L), c(1L, 3L, 4L))
ffi_extract_bits64(packed, 1L, 3L)  # 5 (mode field)
#> [1] 5
```
