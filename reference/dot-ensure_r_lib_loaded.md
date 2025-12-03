# Ensure R shared library is loaded

On Windows, R.dll may not be automatically available for symbol lookup.
This function attempts to load it if needed.

## Usage

``` r
.ensure_r_lib_loaded(verbose = FALSE)
```

## Arguments

- verbose:

  Print messages about loading

## Value

Invisibly returns TRUE if R symbols are available
