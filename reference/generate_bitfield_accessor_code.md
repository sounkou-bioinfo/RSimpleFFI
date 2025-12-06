# Generate bit-field accessor code

Generate bit-field accessor code

## Usage

``` r
generate_bitfield_accessor_code(struct_name, bitfield_specs, typedefs = NULL)
```

## Arguments

- struct_name:

  Name of the struct

- bitfield_specs:

  Character vector of "'name : width'" strings

- typedefs:

  Optional named list mapping typedef aliases to canonical C type
  strings. When provided, the generator will attempt to resolve
  per-field base types through this map (e.g., list(myint = "int32_t")).
  Useful when parsing headers that use typedefs, so the code can
  auto-detect a common base_type to emit in the generated call to
  `ffi_create_bitfield_accessors`.

## Value

Character string with accessor code
