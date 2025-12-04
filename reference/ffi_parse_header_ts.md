# Parse C header using tree-sitter (if available)

Parse C header using tree-sitter (if available)

## Usage

``` r
ffi_parse_header_ts(header_file, includes = NULL, use_treesitter = TRUE)
```

## Arguments

- header_file:

  Path to C header file

- includes:

  Additional include directories (used for TCC preprocessing)

- use_treesitter:

  If TRUE, use tree-sitter parser. If FALSE or if treesitter
  unavailable, fall back to regex.

## Value

List with parsed components (file, defines, structs, unions, enums,
functions, typedefs)
