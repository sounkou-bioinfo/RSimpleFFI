# Set field value in FFI structure

Set field value in FFI structure

## Usage

``` r
ffi_set_field(ptr, field, value, struct_type, ...)
```

## Arguments

- ptr:

  External pointer to structure

- field:

  Character field name or integer field index

- value:

  Value to set

- struct_type:

  StructType object

- ...:

  Not used; required for S7 generic dispatch.

## Value

Updated pointer
