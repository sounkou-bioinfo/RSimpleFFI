# Parse C header file and create structured result

Parse C header file and create structured result

## Usage

``` r
ffi_parse_header(header_file, includes = NULL, use_treesitter = TRUE)
```

## Arguments

- header_file:

  Path to C header file

- includes:

  Additional include directories

- use_treesitter:

  If TRUE (default), use tree-sitter parser when available. If FALSE,
  use regex parser.

## Value

List with parsed components (file, defines, structs, unions, enums,
functions, typedefs)
