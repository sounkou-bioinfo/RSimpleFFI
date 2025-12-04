# Get read-only data pointer from R vector with GC protection

Like
[`data_ptr()`](https://sounkou-bioinfo.github.io/RSimpleFFI/reference/data_ptr.md)
but attempts read-only access first (for ALTREP support). If the ALTREP
implementation doesn't provide direct access, falls back to
materializing the data (which may allocate memory).

## Usage

``` r
data_ptr_ro(x)
```

## Arguments

- x:

  An R vector

## Value

External pointer to the data
