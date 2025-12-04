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

## Examples

``` r
if (FALSE) { # \dontrun{
x <- as.double(1:10)
ptr <- data_ptr(x)
# ptr points to the double* array
# Safe to pass to C functions expecting double*
} # }
```
