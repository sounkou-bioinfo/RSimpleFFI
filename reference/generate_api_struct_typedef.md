# Generate struct typedef from field types

Generate struct typedef from field types

## Usage

``` r
generate_api_struct_typedef(struct_name, field_names, field_types = NULL)
```

## Arguments

- struct_name:

  Name of the C struct

- field_names:

  Character vector of field names

- field_types:

  Character vector of C type names (e.g., "int", "double")

## Value

Character string containing typedef
