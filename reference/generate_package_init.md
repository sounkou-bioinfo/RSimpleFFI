# Generate .onLoad/.onUnload for package

Creates the zzz.R file content for loading external libraries

## Usage

``` r
generate_package_init(
  library_name,
  package_name,
  library_path = NULL,
  use_system_lib = TRUE
)
```

## Arguments

- library_name:

  Name of the shared library (e.g., "mylib")

- package_name:

  Name of the R package

- library_path:

  Optional: specific path to library, or NULL for system search

- use_system_lib:

  Logical: search system library paths

## Value

Character string with zzz.R content

## Examples

``` r
if (FALSE) { # \dontrun{
# Generate for system library
code <- generate_package_init("mylib", "MyRPackage", use_system_lib = TRUE)
writeLines(code, "R/zzz.R")

# Generate for bundled library
code <- generate_package_init("mylib", "MyRPackage", use_system_lib = FALSE)
} # }
```
