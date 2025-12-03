# Generate R function wrapper from parsed function

Generate R function wrapper from parsed function

## Usage

``` r
generate_function_wrapper(func_def, typedefs = NULL)
```

## Arguments

- func_def:

  Function definition (row from functions data.frame)

- typedefs:

  Named character vector of typedefs (optional). Used to resolve
  typedef'd types like SEXPTYPE to their underlying FFI types.

## Value

Character vector with R code
