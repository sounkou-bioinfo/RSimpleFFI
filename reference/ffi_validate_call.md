# Validate FFI call prerequisites

Performs comprehensive validation of FFI call inputs before making the
call. This helps diagnose issues that would otherwise cause crashes.

## Usage

``` r
ffi_validate_call(cif, symbol, args = list(), verbose = FALSE)
```

## Arguments

- cif:

  CIF object defining the call interface

- symbol:

  NativeSymbol object for the function

- args:

  List of arguments to pass

- verbose:

  Logical; if TRUE, print diagnostic information

## Value

A list with validation results:

- valid:

  Logical; TRUE if all checks pass

- errors:

  Character vector of error messages (empty if valid)

- warnings:

  Character vector of warning messages

## Details

This function checks:

- CIF and symbol pointers are not NULL

- Argument count matches CIF specification

- No NA values in arguments (unless explicitly allowed)

- Pointer arguments are not NULL (when applicable)

Note that even with all checks passing, crashes can still occur if:

- The C function signature doesn't match the CIF

- Pointer arguments point to invalid memory

- Buffer sizes are incorrect

- The C function itself has bugs

## Examples

``` r
if (FALSE) { # \dontrun{
cif <- ffi_cif(ffi_int(), ffi_int(), ffi_int())
sym <- ffi_symbol("add_ints")
result <- ffi_validate_call(cif, sym, list(1L, 2L))
if (result$valid) {
  ffi_call(cif, sym, 1L, 2L)
}
} # }
```
