# Generate complete helper module for a struct (API mode)

Combines constructor and offset extractor into a complete C file.

## Usage

``` r
generate_api_struct_helpers(
  struct_name,
  field_names,
  field_types = NULL,
  header_includes = NULL,
  prefix = "rffi_"
)
```

## Arguments

- struct_name:

  Name of the C struct

- field_names:

  Character vector of field names

- field_types:

  Character vector of C type names (defaults to "int" for all)

- header_includes:

  Character vector of header files to include

- prefix:

  Prefix for generated function names (default "rffi\_")

## Value

Character string containing complete C code
