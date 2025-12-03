# Generate complete package from header files

Creates all necessary R files for a package wrapping a C library

## Usage

``` r
generate_package_from_headers(
  header_files,
  package_name,
  library_name,
  output_dir = "R",
  use_system_lib = TRUE,
  include_helpers = TRUE
)
```

## Arguments

- header_files:

  Character vector of header file paths

- package_name:

  Name of the R package

- library_name:

  Name of the shared library

- output_dir:

  Directory to write generated files

- use_system_lib:

  Logical: search system library paths

- include_helpers:

  Logical: include allocation helper functions

## Value

Invisibly returns list of generated files

## Examples

``` r
if (FALSE) { # \dontrun{
generate_package_from_headers(
  header_files = c("mylib.h", "mylib_utils.h"),
  package_name = "MyRPackage",
  library_name = "mylib",
  output_dir = "MyRPackage/R",
  use_system_lib = TRUE
)
} # }
```
