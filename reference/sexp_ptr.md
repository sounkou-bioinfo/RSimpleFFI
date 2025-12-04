# Get SEXP pointer from R object with GC protection

Returns an external pointer to the SEXP (R object header) while ensuring
the object won't be garbage collected as long as the pointer exists.

## Usage

``` r
sexp_ptr(x)
```

## Arguments

- x:

  Any R object

## Value

External pointer to the SEXP, with finalizer to release protection

## Examples

``` r
if (FALSE) { # \dontrun{
x <- 1:10
ptr <- sexp_ptr(x)
# ptr is now safe to pass to C functions expecting SEXP
# When ptr is garbage collected, the protection is released
} # }
```
