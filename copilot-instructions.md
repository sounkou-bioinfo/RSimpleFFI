# RSimpleFFI - Copilot Instructions

## Project Overview

RSimpleFFI is an R package providing a Foreign Function Interface (FFI)
using libffi and S7 classes. It enables calling C functions from R with
type-safe wrappers around native pointers.

## Architecture

### Core Components

- **R Layer** (`R/`): S7 class definitions and R wrappers
  - `00-types.R`: Core classes (`FFIType`, `StructType`, `ArrayType`,
    `CIF`, `NativeSymbol`, `FFIClosure`)
  - `01-functions.R`: FFI call interface (`ffi_cif`, `ffi_cif_var`,
    `ffi_symbol`, `ffi_call`, `ffi_function`)
  - `04-alloc.R`: Memory allocation (`ffi_alloc`, `ffi_get_element`)
  - `dll_loading.R`: Dynamic library management (`dll_load`,
    `dll_symbol`)
- **C Layer** (`src/`):
  - `simple_ffi.c`: Core libffi bindings, type mappings, memory
    management
  - `init.c`: Symbol registration for
    [`.Call()`](https://rdrr.io/r/base/CallExternal.html) interface
  - `test_functions.c`: C functions for testing (also serve as usage
    examples)

### Key Patterns

**Type Creation Pattern** - All FFI types are created via factory
functions:

``` r
# Basic types
int_t <- ffi_int()
double_t <- ffi_double()
# Struct types
Point <- ffi_struct(x = ffi_int(), y = ffi_int())
# Array types
arr_type <- ffi_array_type(ffi_int(), 4L)
```

**Call Pattern** - Three steps: type → CIF → call:

``` r
cif <- ffi_cif(ffi_int(), ffi_int(), ffi_int())  # return, arg1, arg2
result <- ffi_call(cif, ffi_symbol("function_name"), arg1, arg2)
# Or use convenience wrapper:
fn <- ffi_function("function_name", ffi_int(), ffi_int(), ffi_int())
result <- fn(arg1, arg2)
```

**Memory Pattern** - Allocate → Fill/Access → (auto-free via GC):

``` r
ptr <- ffi_alloc(type, n)
ffi_fill_typed_buffer(ptr, values, type)
result <- ffi_copy_array(ptr, n, type)
```

## Development Workflow

### Build Commands

``` bash
R CMD build .                    # Build package tarball
R CMD check --as-cran *.tar.gz   # Run full CRAN checks (use built tarball)
```

### Testing

``` bash
R -e "devtools::test()"          # Run testthat tests
R -e "testthat::test_file('tests/testthat/test-functions.R')"  # Single test file
```

### Documentation

``` bash
R -e "devtools::document()"      # Generate Rd files from roxygen2
R -e "rmarkdown::render('README.Rmd')"  # Render README.Rmd to README.md
R -e "pkgdown::build_site()"     # Build documentation website
```

## Code Conventions

### S7 Classes

- All classes use
  [`S7::new_class()`](https://rconsortium.github.io/S7/reference/new_class.html)
  with `package = "RSimpleFFI"`
- Classes have `validator` functions for property checks
- Use `S7::S7_inherits(obj, ClassName)` for type checking
- Method dispatch via
  [`S7::new_generic()`](https://rconsortium.github.io/S7/reference/new_generic.html)
  and
  [`S7::method()`](https://rconsortium.github.io/S7/reference/method.html)

### Documentation

- **Always use roxygen2** for all R function documentation
- Run `devtools::document()` after adding/modifying roxygen comments
- Use `@export` for public functions, `@keywords internal` for internal
  ones

### C Interface

- All R-callable C functions are prefixed with `R_` (e.g., `R_ffi_call`)
- Use `.Call("R_function_name", ...)` for R-to-C calls
- External pointers must be validated with
  `inherits(ptr, "externalptr")`
- Register all C symbols in `src/init.c`

### Naming Conventions

- Public R functions: `ffi_*` prefix (e.g., `ffi_int`, `ffi_call`,
  `ffi_alloc`)
- DLL functions: `dll_*` prefix (e.g., `dll_load`, `dll_symbol`)
- Type factory functions: `ffi_<typename>()` returns `FFIType`

### README.Rmd Style

- Keep examples succinct and focused
- **Never use [`cat()`](https://rdrr.io/r/base/cat.html) or
  [`message()`](https://rdrr.io/r/base/message.html) for output** - let
  code results speak for themselves
- No verbose decorations or formatting in examples

### Output in Package Code

- **Never use [`cat()`](https://rdrr.io/r/base/cat.html) or
  [`print()`](https://rdrr.io/r/base/print.html) in package functions**
- Use [`message()`](https://rdrr.io/r/base/message.html) only when
  `verbose = TRUE` is passed
- Let return values speak for themselves

## Adding New Features

### Adding a New Type

1.  Add type definition in `src/simple_ffi.c` (in `type_map` array)
2.  Add factory function in `R/00-types.R`:

``` r
#' @export
ffi_newtype <- function() {
  create_builtin_type("newtype")
}
```

3.  Export in `NAMESPACE` (run `devtools::document()`)
4.  Add tests in `tests/testthat/`

### Adding Test C Functions

1.  Add function in `src/test_functions.c`
2.  Declare in `src/init.c` if needed for symbol registration
3.  Reference via `ffi_symbol("test_function_name")` in tests

## Platform Notes

- **Unix**: libffi is vendored and built from source during package
  install (see `configure`)
- **Windows**: Requires RTools with libffi and pkg-config pre-installed
  (see `configure.win`)
- **macOS**: Uses `-Wl,-rpath,@loader_path` for libffi linking

## Key Files Reference

| Path                               | Purpose                             |
|------------------------------------|-------------------------------------|
| `R/00-types.R`                     | Core S7 class definitions           |
| `R/01-functions.R`                 | FFI call interface functions        |
| `src/simple_ffi.c`                 | libffi C bindings                   |
| `src/init.c`                       | C symbol registration               |
| `src/test_functions.c`             | Test C functions (example patterns) |
| `tests/testthat/test-functions.R`  | Core FFI call tests                 |
| `tests/testthat/test-structures.R` | Struct handling tests               |
