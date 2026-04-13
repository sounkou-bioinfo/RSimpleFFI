# Generate src/struct_helpers.c for R package

Creates all struct accessor functions for compilation into the package

## Usage

``` r
generate_package_struct_helpers_c(
  parsed_headers,
  header_includes = NULL,
  prefix = "rffi_"
)
```

## Arguments

- parsed_headers:

  List of parsed header structures

- header_includes:

  Character vector of header files to include

- prefix:

  Prefix for C function names (default "rffi\_")

## Value

Character string containing src/struct_helpers.c code
