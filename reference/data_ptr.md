# Get data pointer from R vector with GC protection

Returns an external pointer to the underlying data (INTEGER, REAL, etc.)
while ensuring the object won't be garbage collected.

## Usage

``` r
data_ptr(x)
```

## Arguments

- x:

  An R vector (integer, double, complex, character, raw, or list)

## Value

External pointer to the data, with finalizer to release protection

## Details

Note: For ALTREP objects (like `1:10`), use
[`data_ptr_ro()`](https://sounkou-bioinfo.github.io/RSimpleFFI/reference/data_ptr_ro.md)
instead, which properly handles deferred materialization.

## Examples

``` r
if (FALSE) { # \dontrun{
x <- c(1.0, 2.0, 3.0) # Regular vector, not ALTREP
ptr <- data_ptr(x)
# ptr points to the double* array
# Safe to pass to C functions expecting double*
} # }
```
