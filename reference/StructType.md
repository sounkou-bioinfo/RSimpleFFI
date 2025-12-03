# FFI Structure Type

FFI Structure Type

## Usage

``` r
StructType(
  name = character(0),
  size = integer(0),
  ref = NULL,
  fields = character(0),
  field_types = list(),
  pack = NULL
)
```

## Arguments

- name:

  Character name of the type

- size:

  Integer size in bytes

- ref:

  External pointer to ffi_type

- fields:

  Character vector of field names

- field_types:

  List of FFIType objects for each field

- pack:

  Integer packing alignment (NULL for default/natural alignment)
