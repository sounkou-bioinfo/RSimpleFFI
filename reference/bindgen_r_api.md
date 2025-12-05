# Generate FFI bindings for R's C API

Parses R's header files (Rinternals.h, R.h, Rmath.h) and generates R
bindings that allow calling R's internal C functions directly via FFI.

## Usage

``` r
bindgen_r_api(
  output_file = NULL,
  headers = c("Rinternals.h", "R.h", "Rmath.h"),
  include_path = R.home("include"),
  load_r_lib = .is_windows,
  verbose = FALSE
)
```

## Arguments

- output_file:

  Path to write the generated R bindings. If NULL, returns the parsed
  results without writing.

- headers:

  Character vector of header names to parse. Default includes
  "Rinternals.h", "R.h", and "Rmath.h".

- include_path:

  Path to R's include directory. Defaults to `R.home("include")`.

- load_r_lib:

  Logical. If TRUE (default on Windows), attempt to load the R shared
  library to ensure symbols are available.

- verbose:

  Logical. If TRUE, print progress messages.

## Value

Invisibly returns a list with parsed results for each header:

- structs:

  Named list of struct definitions

- functions:

  Named list of function signatures

- typedefs:

  Named list of typedef mappings

- enums:

  Named list of enum definitions

## Details

On Unix-like systems (Linux, macOS), R's symbols are typically available
in the running process. On Windows, the R.dll may need to be explicitly
loaded. Set `load_r_lib = TRUE` to handle this automatically.

The generated bindings create wrapper functions that use
[`ffi_function()`](https://sounkou-bioinfo.github.io/RSimpleFFI/reference/ffi_function.md)
to call the underlying C functions. Each wrapper includes roxygen2
documentation with parameter types and return values.

## Available Headers

- Rinternals.h:

  Core R internals: SEXP manipulation, memory management, type checking
  functions (Rf_isInteger, Rf_length, etc.)

- R.h:

  Main R header, includes standard utilities

- Rmath.h:

  Statistical distribution functions (dnorm, pnorm, qnorm, gamma, beta,
  etc.)

## See also

[ffi_parse_header()](https://sounkou-bioinfo.github.io/RSimpleFFI/reference/ffi_parse_header.md),
[generate_r_bindings()](https://sounkou-bioinfo.github.io/RSimpleFFI/reference/generate_r_bindings.md),
[ffi_function()](https://sounkou-bioinfo.github.io/RSimpleFFI/reference/ffi_function.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Generate bindings to a file
bindgen_r_api(output_file = "r_api_bindings.R")

# Parse without writing (for inspection)
result <- bindgen_r_api(verbose = TRUE)
names(result$Rinternals$functions)

# Generate only Rmath bindings
bindgen_r_api(
    output_file = "rmath_bindings.R",
    headers = "Rmath.h"
)

# After sourcing generated bindings:
# source("r_api_bindings.R")
# r_Rf_dnorm4(0, 0, 1, 0L)  # same as dnorm(0, 0, 1)
} # }
```
