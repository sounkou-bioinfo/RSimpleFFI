# FFI Closure - R function as C callback

A closure wraps an R function so it can be used as a callback from C
code. The closure has an associated CIF that describes the function
signature.

## Usage

``` r
FFIClosure(
  r_function = function() NULL,
  cif = CIF(),
  ref = NULL,
  func_ptr = NULL
)
```

## Arguments

- r_function:

  The R function to wrap

- cif:

  CIF object describing the callback signature

- ref:

  External pointer to the closure

- func_ptr:

  External pointer to the executable function

## Value

An FFIClosure object
