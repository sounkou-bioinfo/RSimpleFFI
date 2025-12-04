# Generate R struct definition from parsed struct

Generate R struct definition from parsed struct

## Usage

``` r
generate_struct_definition(struct_name, struct_def, typedefs = NULL)
```

## Arguments

- struct_name:

  Name of the struct

- struct_def:

  Struct definition from parsed header

- typedefs:

  Optional data frame of typedefs to resolve type aliases

## Value

Character vector with R code
