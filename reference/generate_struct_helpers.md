# Generate struct helper functions (allocator, from_list, to_list)

Creates convenience functions for working with a struct type:

- `new_<struct>()`: Allocate a new struct, optionally initialize from
  values

- `<struct>_to_list()`: Convert struct pointer to R list

## Usage

``` r
generate_struct_helpers(struct_name, field_names)
```

## Arguments

- struct_name:

  Name of the struct (should match the ffi_struct variable name)

- field_names:

  Character vector of field names

## Value

Character string with R code for helper functions
