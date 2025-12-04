# Generate R bindings from parsed header

Generate R bindings from parsed header

## Usage

``` r
generate_r_bindings(parsed_header, output_file = NULL, verbose = FALSE)
```

## Arguments

- parsed_header:

  Parsed header object from ffi_parse_header()

- output_file:

  Optional file to write code to

- verbose:

  If TRUE, print progress messages

## Value

Character vector with all generated R code
