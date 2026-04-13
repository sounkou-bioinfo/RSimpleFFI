# Convert field pointer to R value

Converts a tagged field pointer to an R value using the type information
stored in the pointer's tag.

## Usage

``` r
ffi_field_to_r(field_ptr)
```

## Arguments

- field_ptr:

  External pointer to field (must be tagged with FFI type)

## Value

Field value converted to appropriate R type
