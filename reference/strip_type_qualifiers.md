# Strip C type qualifiers from a type string

Removes const, volatile, restrict, \_Atomic, and related qualifiers from
a C type string, normalizing whitespace.

## Usage

``` r
strip_type_qualifiers(type_str)
```

## Arguments

- type_str:

  Character string containing a C type

## Value

Cleaned type string with qualifiers removed
