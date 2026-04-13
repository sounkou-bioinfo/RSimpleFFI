# Generate complete package from header files Generate complete package from header files

Creates all necessary R files for a package wrapping a C library.
Generates a proper R package structure with DESCRIPTION, NAMESPACE, and
R code in the R/ subfolder. Uses templates from inst/templates/.

## Usage

``` r
generate_package_from_headers(
  header_files,
  package_name,
  library_name,
  output_dir = package_name,
  library_path = NULL,
  use_system_lib = TRUE,
  include_helpers = TRUE,
  use_api_mode = FALSE,
  authors_r = NULL,
  title = NULL,
  description = NULL
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

  Directory to create the package (package root)

- library_path:

  Optional: full path to shared library (for custom installs)

- use_system_lib:

  Logical: search system library paths

- include_helpers:

  Logical: include allocation helper functions

- use_api_mode:

  Logical: use API mode (compile struct helpers into package). When
  TRUE, generates src/ directory with init.c and struct_helpers.c for
  compiled struct accessors. When FALSE (default), uses ABI mode with
  runtime offset calculation. API mode is required for structs with
  bitfields.

- authors_r:

  Authors@R field for DESCRIPTION (R code string). Default creates a
  placeholder person().

- title:

  Package title (default: auto-generated)

- description:

  Package description (default: auto-generated)

## Value

Invisibly returns list of generated files

## Examples

``` r
if (FALSE) { # \dontrun{
# ABI mode (default) - runtime offset calculation
generate_package_from_headers(
  header_files = "mylib.h",
  package_name = "MyRPackage",
  library_name = "mylib",
  use_api_mode = FALSE
)

# API mode - compiled struct helpers (better for bitfields)
generate_package_from_headers(
  header_files = c("mylib.h", "mylib_utils.h"),
  package_name = "MyRPackage",
  library_name = "mylib",
  output_dir = "MyRPackage",
  use_api_mode = TRUE,
  use_system_lib = TRUE,
  include_helpers = TRUE,
  authors_r = 'person("John", "Doe", email = "john@example.com", role = c("aut", "cre"))',
  title = "FFI Bindings to mylib",
  description = "Auto-generated FFI bindings for mylib with compiled struct helpers."
)
} # }
```
