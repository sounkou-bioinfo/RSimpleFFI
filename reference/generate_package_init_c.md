# Generate src/init.c for R package

Creates the C function registration code for R CMD INSTALL

## Usage

``` r
generate_package_init_c(
  struct_names,
  bitfield_funcs = list(),
  prefix = "rffi_"
)
```

## Arguments

- struct_names:

  Character vector of struct names

- prefix:

  Prefix for C function names (default "rffi\_")

## Value

Character string containing src/init.c code
