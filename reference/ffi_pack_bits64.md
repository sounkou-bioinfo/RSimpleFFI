# Pack bit-fields into a 64-bit value

Packs multiple values into a single 64-bit integer (returned as double)
according to specified bit widths. This version uses C code for full
64-bit support.

## Usage

``` r
ffi_pack_bits64(values, widths)
```

## Arguments

- values:

  Integer vector of values to pack

- widths:

  Integer vector of bit widths for each value

## Value

Packed value as double (for 64-bit range)

## Details

Values are packed from LSB to MSB (least significant bit to most
significant bit). Each value is masked to its specified width and
shifted into position. Uses C implementation for full 64-bit support (R
integers are only 32-bit).

## Examples

``` r
# Pack three bit-fields: enabled (1 bit), mode (3 bits), priority (4 bits)
packed <- ffi_pack_bits64(c(1L, 5L, 12L), c(1L, 3L, 4L))

# Works with values > 32 bits total
large_packed <- ffi_pack_bits64(c(1L, 0xFFFFFFFFL), c(1L, 32L))
#> Warning: NAs introduced by coercion to integer range
```
