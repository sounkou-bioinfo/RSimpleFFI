# Get summary of R API bindings

Quick summary of what's available in R's C API headers.

## Usage

``` r
bindgen_r_api_summary(include_path = R.home("include"))
```

## Arguments

- include_path:

  Path to R's include directory

## Value

Data frame with header info

## Examples

``` r
if (FALSE) { # \dontrun{
bindgen_r_api_summary()
} # }
```
