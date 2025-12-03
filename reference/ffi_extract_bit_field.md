# Extract a single bit-field from a packed value

Extracts a single bit-field value from a packed integer at a specified
bit offset and width.

## Usage

``` r
ffi_extract_bit_field(packed_value, bit_offset, bit_width)
```

## Arguments

- packed_value:

  Integer value containing packed bit-fields

- bit_offset:

  Bit offset from LSB (0-based)

- bit_width:

  Number of bits in the field

## Value

Extracted integer value

## Examples

``` r
# Extract 3-bit mode field at bit offset 1 from value 0x65
ffi_extract_bit_field(0x65L, 1L, 3L) # 5
#> [1] 2

# Extract 4-bit priority field at bit offset 4
ffi_extract_bit_field(0x65L, 4L, 4L) # 6
#> [1] 6
```
