# Check if a typedef can be fully resolved to a real type

Recursively follows typedef chains to see if the final base type can be
mapped to an FFI type. Returns FALSE for unresolvable types like
`__builtin_va_list`.

## Usage

``` r
can_resolve_typedef(
  base_type,
  known_typedefs,
  known_structs = character(),
  visited = character()
)
```

## Arguments

- base_type:

  The underlying C type string

- known_typedefs:

  Named character vector of typedefs

- known_structs:

  Character vector of known struct names

- visited:

  Set of already-visited typedefs (to detect cycles)

## Value

TRUE if resolvable, FALSE otherwise
