# FFI Array Type

FFI Array Type

## Usage

``` r
ArrayType(
  name = character(0),
  size = integer(0),
  ref = NULL,
  element_type = FFIType(),
  length = integer(0)
)
```

## Arguments

- name:

  Character name of the type

- size:

  Integer size in bytes

- ref:

  External pointer to ffi_type

- element_type:

  FFIType of array elements

- length:

  Integer length of array

## Value

An ArrayType object
