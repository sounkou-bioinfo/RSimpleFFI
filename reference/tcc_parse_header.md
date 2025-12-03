# Parse C header file and extract all declarations

Parse C header file and extract all declarations

## Usage

``` r
tcc_parse_header(header_file, includes = NULL)
```

## Arguments

- header_file:

  Path to C header file

- includes:

  Additional include directories

## Value

List with components: functions, structs, unions, enums, defines,
typedefs
