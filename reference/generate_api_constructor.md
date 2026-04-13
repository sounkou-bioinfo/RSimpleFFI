# Constructor function for a struct (API mode)

Creates C code for allocating and initializing a struct with default
values.

## Usage

``` r
generate_api_constructor(struct_name, prefix = "rffi_")
```

## Arguments

- struct_name:

  Name of the C struct

- prefix:

  Prefix for generated function names (default "rffi\_")

## Value

Character string containing C code
