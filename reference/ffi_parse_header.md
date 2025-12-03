# Parse C header file and create structured result

Parse C header file and create structured result

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

List with parsed components (file, defines, structs, functions)
