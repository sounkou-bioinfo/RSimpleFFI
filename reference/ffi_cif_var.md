# Prepare FFI call interface for variadic functions

Creates a CIF for calling C functions with variable arguments (varargs).
Unlike regular CIFs, variadic CIFs must specify the types of ALL
arguments for each specific call, including the variadic ones.

## Usage

``` r
ffi_cif_var(return_type, nfixedargs, ...)
```

## Arguments

- return_type:

  FFIType for return value

- nfixedargs:

  Number of fixed arguments (before the ...)

- ...:

  FFIType objects for ALL arguments (fixed + variadic)

## Value

CIF object

## Details

Due to C calling conventions, variadic arguments undergo "default
argument promotions": float becomes double, and small integers (char,
short) become int. You must use ffi_int() or ffi_double() for variadic
arguments, not smaller types.

## Examples

``` r
if (FALSE) { # \dontrun{
# Call a varargs function: test_varargs_sum(int nargs, ...)
# First argument (nargs) is fixed, rest are variadic integers
sym <- ffi_symbol("test_varargs_sum")

# Call with 3 variadic int arguments
cif <- ffi_cif_var(ffi_double(),
  nfixedargs = 1L,
  ffi_int(), ffi_int(), ffi_int(), ffi_int()
)
result <- ffi_call(cif, sym, 3L, 10L, 20L, 30L) # returns 60
} # }
```
