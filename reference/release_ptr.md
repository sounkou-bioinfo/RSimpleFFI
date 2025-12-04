# Manually release a protected pointer

Normally not needed - the finalizer handles this automatically. Use only
if you need to release protection early.

## Usage

``` r
release_ptr(ptr)
```

## Arguments

- ptr:

  A protected pointer from sexp_ptr() or data_ptr()

## Value

NULL invisibly
