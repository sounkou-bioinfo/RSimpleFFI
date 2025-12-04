# Get read-only data pointer from R vector with GC protection

Like
[`data_ptr()`](https://sounkou-bioinfo.github.io/RSimpleFFI/reference/data_ptr.md)
but returns a const pointer (for ALTREP support). Use this when you only
need to read the data, not modify it.

## Usage

``` r
data_ptr_ro(x)
```

## Arguments

- x:

  An R vector

## Value

External pointer to the const data
