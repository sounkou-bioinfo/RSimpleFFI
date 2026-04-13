# Generate R wrapper code that calls compiled C accessors

Creates R functions that use .Call() to invoke compiled struct helpers.

## Usage

``` r
generate_api_r_wrappers(
  struct_name,
  field_names,
  prefix = "rffi_",
  package_name = NULL
)
```

## Arguments

- struct_name:

  Name of the struct

- field_names:

  Character vector of field names

- prefix:

  Prefix for C function names (default "rffi\_")

- package_name:

  Name of the package (for .Call interface)

## Value

Character string containing R code
