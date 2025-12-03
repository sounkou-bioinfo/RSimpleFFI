# Extract \#define macros from C header file or preprocessed lines

Extract \#define macros from C header file or preprocessed lines

## Usage

``` r
tcc_extract_defines(header_file = NULL, preprocessed_lines = NULL)
```

## Arguments

- header_file:

  Path to C header file (optional if preprocessed_lines provided)

- preprocessed_lines:

  Character vector from tcc_preprocess() (optional)

## Value

Named list of macro definitions
