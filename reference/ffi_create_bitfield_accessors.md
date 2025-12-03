# Create accessor functions for a bit-field structure

Generates getter and setter functions for a C structure with bit-fields,
allowing easy manipulation of packed bit-field values.

## Usage

``` r
ffi_create_bitfield_accessors(field_specs, base_type = ffi_uint32())
```

## Arguments

- field_specs:

  Named list where names are field names and values are bit widths
  (integers)

- base_type:

  FFI type for the packed representation (default: ffi_uint32())

## Value

List with `pack`, `unpack`, `get`, and `set` functions

## Examples

``` r
# Define a bit-field structure
# C equivalent:
# struct Flags {
#   unsigned int enabled : 1;
#   unsigned int mode : 3;
#   unsigned int priority : 4;
# };

flags_accessors <- ffi_create_bitfield_accessors(
  list(enabled = 1L, mode = 3L, priority = 4L)
)

# Pack values
packed <- flags_accessors$pack(list(enabled = 1L, mode = 5L, priority = 12L))

# Unpack to list
flags_accessors$unpack(packed)
#> $enabled
#> [1] 1
#> 
#> $mode
#> [1] 5
#> 
#> $priority
#> [1] 12
#> 
# $enabled:  1
# $mode:     5
# $priority: 12

# Get a single field
flags_accessors$get(packed, "mode")  # 5
#> [1] 5

# Set a single field
new_packed <- flags_accessors$set(packed, "mode", 7L)
flags_accessors$get(new_packed, "mode")  # 7
#> [1] 7
```
