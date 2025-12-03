# Set a single bit-field in a packed value

Updates a single bit-field in a packed integer at a specified bit offset
and width, returning the modified packed value.

## Usage

``` r
ffi_set_bit_field(packed_value, new_value, bit_offset, bit_width)
```

## Arguments

- packed_value:

  Integer value containing packed bit-fields

- new_value:

  New value for the bit-field

- bit_offset:

  Bit offset from LSB (0-based)

- bit_width:

  Number of bits in the field

## Value

Modified packed integer value

## Examples

``` r
# Set 3-bit mode field at bit offset 1 to value 7
ffi_set_bit_field(0x65L, 7L, 1L, 3L) # 0x6F
#> [1] 111

# Set 1-bit enabled field at bit offset 0 to 0
ffi_set_bit_field(0x65L, 0L, 0L, 1L) # 0x64
#> [1] 100
```
