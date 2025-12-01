# Free memory pointed to by an external pointer

Explicitly frees memory that was allocated by C code and returned as a
pointer. Use this when you know the pointer was allocated with
malloc/calloc and it's your responsibility to free it.

## Usage

``` r
ffi_free(ptr)
```

## Arguments

- ptr:

  External pointer to free

## Value

NULL invisibly

## Details

**When to use this:**

- Pointers returned from C functions that allocate memory (e.g.,
  `strdup`, `malloc`)

- When C documentation says "caller must free"

**When NOT to use this:**

- Pointers allocated via
  [`ffi_alloc()`](https://sounkou-bioinfo.github.io/RSimpleFFI/reference/alloc.md)
  (auto-freed by R's GC)

- Static or global pointers from C

- Pointers into existing structures

- Pointers that C will free itself

Calling `ffi_free()` on an already-freed pointer or invalid pointer will
cause undefined behavior (likely crash).

## Examples

``` r
if (FALSE) { # \dontrun{
# C function that allocates and returns a string
strdup_fn <- ffi_function("strdup", ffi_pointer(), ffi_string())
ptr <- strdup_fn("hello")
# ... use ptr ...
ffi_free(ptr) # We must free because strdup allocates
} # }
```
