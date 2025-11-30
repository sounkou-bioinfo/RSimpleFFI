# Create typed external pointer

Creates an external pointer with a specific type tag for better type
safety, similar to Rffi's approach.

## Usage

``` r
make_typed_pointer(ptr, type_name)
```

## Arguments

- ptr:

  External pointer

- type_name:

  Character string describing the pointer type

## Value

External pointer with type tag
