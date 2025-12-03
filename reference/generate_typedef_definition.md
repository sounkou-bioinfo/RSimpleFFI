# Generate R typedef alias from parsed typedef

Creates R code that maps a typedef alias to its underlying FFI type.
Handles simple type aliases (e.g., typedef int my_int) and struct
typedefs.

## Usage

``` r
generate_typedef_definition(
  alias_name,
  base_type,
  known_structs = character(),
  known_typedefs = character()
)
```

## Arguments

- alias_name:

  Name of the typedef alias

- base_type:

  The underlying C type string

- known_structs:

  Character vector of known struct names (for struct type resolution)

- known_typedefs:

  Named character vector of already-processed typedefs (for chained
  resolution)

## Value

Character string with R code, or NULL if type cannot be mapped
