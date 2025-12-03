# Get summary of R API bindings

Quick summary of what's available in R's C API headers, including all
headers in the main include directory and the R_ext subdirectory.

## Usage

``` r
bindgen_r_api_summary(include_path = R.home("include"))
```

## Arguments

- include_path:

  Path to R's include directory

## Value

Data frame with header info including name, exists flag, size, and
category

## Examples

``` r
if (FALSE) { # \dontrun{
bindgen_r_api_summary()
} # }
```
