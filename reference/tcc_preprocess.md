# Preprocess C header file using embedded TCC

Preprocess C header file using embedded TCC

## Usage

``` r
tcc_preprocess(header_file, includes = NULL, keep_defines = FALSE)
```

## Arguments

- header_file:

  Path to C header file

- includes:

  Additional include directories

- keep_defines:

  Keep \#define directives (not supported by -E alone)

## Value

Character vector of preprocessed lines
