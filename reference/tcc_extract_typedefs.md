# Extract simple typedef aliases from preprocessed C code

Parses typedefs of the form "typedef \<base_type\> ;" and returns a
named list mapping alias names to their underlying types. This excludes
struct/union/enum typedefs which are handled separately.

## Usage

``` r
tcc_extract_typedefs(preprocessed_lines)
```

## Arguments

- preprocessed_lines:

  Character vector from tcc_preprocess()

## Value

Named character vector mapping alias -\> base_type

## Examples

``` r
if (FALSE) { # \dontrun{
pp <- tcc_preprocess("myheader.h")
typedefs <- tcc_extract_typedefs(pp)
# typedefs might contain: c(my_int = "int", real_t = "float", ...)
} # }
```
