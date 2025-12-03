# FFI Enumeration Type

FFI Enumeration Type

## Usage

``` r
EnumType(
  name = character(0),
  size = integer(0),
  ref = NULL,
  values = integer(0),
  underlying_type = FFIType()
)
```

## Arguments

- name:

  Character name of the type

- size:

  Integer size in bytes

- ref:

  External pointer to ffi_type

- values:

  Named integer vector of enum values

- underlying_type:

  FFIType for the underlying integer type
