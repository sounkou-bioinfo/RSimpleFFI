# Get field pointer from struct (internal helper)

Returns an external pointer to a field within a struct, tagged with the
field's FFI type information for later type-aware operations.

## Usage

``` r
ffi_get_field_ptr(struct_ptr, field_name, struct_type)
```

## Arguments

- struct_ptr:

  External pointer to the struct

- field_name:

  Name of the field to access

- struct_type:

  Struct type metadata (contains field offsets and types)

## Value

External pointer to the field, tagged with FFI type
