# Get struct type name from a struct_specifier node

Extracts just the struct name (e.g., "struct Foo") from a
struct_specifier node, without including the full body definition if
present. For anonymous structs, returns "struct" only.

## Usage

``` r
ts_get_struct_type_name(struct_node)
```

## Arguments

- struct_node:

  A struct_specifier node

## Value

Character string with the struct type name
