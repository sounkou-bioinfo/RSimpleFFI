# Generate bitfield accessor functions for a struct (API mode)

Creates C code for getting/setting bitfield values using compile-time
offset detection and bit operations.

## Usage

``` r
generate_api_bitfield_accessors(struct_name, bitfield_info, prefix = "rffi_")
```

## Arguments

- struct_name:

  Name of the C struct

- bitfield_info:

  List with elements: name, clean_name, width

- prefix:

  Prefix for generated function names

## Value

Character string containing C code
