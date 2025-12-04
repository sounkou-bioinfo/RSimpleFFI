# Parse C header file and create structured result

Uses tree-sitter for robust AST-based parsing of C headers.

## Usage

``` r
ffi_parse_header(header_file, includes = NULL)
```

## Arguments

- header_file:

  Path to C header file

- includes:

  Additional include directories

## Value

List with parsed components (file, defines, structs, unions, enums,
functions, typedefs)
