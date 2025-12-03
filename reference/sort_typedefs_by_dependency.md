# Sort typedefs by dependency order (topological sort)

Ensures that typedef-of-typedef chains are ordered correctly, with base
types defined before types that depend on them.

## Usage

``` r
sort_typedefs_by_dependency(typedefs)
```

## Arguments

- typedefs:

  Named character vector where names are aliases and values are base
  types

## Value

Character vector of typedef names in dependency order
