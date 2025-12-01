# Field Information Class

Represents metadata about a single field in a structure.

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

## Slots

- `name`:

  Character name of the field

- `type`:

  FFIType of the field

- `offset`:

  Integer byte offset within structure

- `size`:

  Integer size of field in bytes

- `index`:

  Integer 1-based field index
