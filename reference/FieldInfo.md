# Field Information Class

Represents metadata about a single field in a structure. Field
Information Class

## Usage

``` r
FieldInfo(
  name = character(0),
  type = FFIType(),
  offset = integer(0),
  size = integer(0),
  index = integer(0)
)
```

## Arguments

- name:

  Character name of the field

- type:

  FFIType of the field

- offset:

  Integer byte offset within structure

- size:

  Integer size of field in bytes

- index:

  Integer 1-based field index

## Value

A FieldInfo object

## Details

Contains metadata about a struct field including its name, type, byte
offset within the structure, size, and index.
