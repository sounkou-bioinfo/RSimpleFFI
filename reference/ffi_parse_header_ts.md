# Parse C header using tree-sitter

Internal function that performs tree-sitter-based parsing. Use
ffi_parse_header() instead which handles dependency checks.

## Usage

``` r
ffi_parse_header_ts(header_file, includes = NULL)
```

## Arguments

- header_file:

  Path to C header file

- includes:

  Additional include directories (used for TCC preprocessing)

## Value

List with parsed components (file, defines, structs, unions, enums,
functions, typedefs)
