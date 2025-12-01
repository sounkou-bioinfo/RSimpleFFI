# Dereference a pointer

Reads the pointer value stored at an address. This is useful for
accessing global variables in shared libraries that are pointers (like
R_GlobalEnv, R_NilValue, etc.).

## Usage

``` r
ffi_deref_pointer(ptr)
```

## Arguments

- ptr:

  External pointer to the address to dereference

## Value

External pointer containing the value at the address

## Examples

``` r
if (FALSE) { # \dontrun{
# Get R_GlobalEnv from libR.so
addr <- getNativeSymbolInfo("R_GlobalEnv")$address
globalenv_sexp <- ffi_deref_pointer(addr)
} # }
```
