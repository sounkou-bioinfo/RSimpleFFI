# Generate offset extractor function for a struct (API mode)

Creates C code that returns all field offsets for a struct using
offsetof(). The compiler computes the offsets, handling alignment,
padding, and bitfields correctly for the target platform.

## Usage

``` r
generate_api_offset_extractor(struct_name, field_names, prefix = "rffi_")
```

## Arguments

- struct_name:

  Name of the C struct (e.g., "Point2D", "hFILE")

- field_names:

  Character vector of field names

- prefix:

  Prefix for generated function names (default "rffi\_")

## Value

Character string containing C code

## Examples

``` r
if (FALSE) { # \dontrun{
code <- generate_api_offset_extractor("Point2D", c("x", "y"))
cat(code)
} # }
```
