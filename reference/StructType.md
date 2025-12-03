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
  pack = NULL,
  has_packed_change = logical(0)
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

- has_packed_change:

  Logical indicating if packing changes field offsets from natural
  alignment. When TRUE, the struct cannot be passed by value to C
  functions (only pointers work) because libffi doesn't support packed
  structs.
