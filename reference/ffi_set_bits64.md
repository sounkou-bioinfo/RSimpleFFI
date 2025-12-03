# Set a single bit-field in a 64-bit packed value

Set a single bit-field in a 64-bit packed value

## Usage

``` r
ffi_set_bits64(packed_value, new_value, bit_offset, bit_width)
```

## Arguments

- packed_value:

  Packed value (as double for 64-bit range)

- new_value:

  New value for the bit-field

- bit_offset:

  Bit offset from LSB (0-based)

- bit_width:

  Number of bits in the field

## Value

Modified packed value as double

## Examples

``` r
packed <- ffi_pack_bits64(c(1L, 5L, 12L), c(1L, 3L, 4L))
new_packed <- ffi_set_bits64(packed, 7L, 1L, 3L)  # Set mode to 7
ffi_extract_bits64(new_packed, 1L, 3L)  # 7
#> [1] 7
```
