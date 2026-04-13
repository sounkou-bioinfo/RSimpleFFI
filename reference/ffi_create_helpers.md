# Create High-Level API Helpers for Struct

This is the main user-facing function for API mode. It generates C code
to compute field offsets using the compiler, compiles it, and returns a
helper object with constructor and field metadata.

## Usage

``` r
ffi_create_helpers(struct_name, field_types, include_dirs = NULL)
```

## Arguments

- struct_name:

  Character string naming the struct (e.g., "Point2D")

- field_types:

  Named list of FFIType objects for each field (e.g., list(x =
  ffi_int(), y = ffi_int()))

- include_dirs:

  Optional character vector of include directories for compilation

## Value

An rffi_struct_helpers S3 object with:

- \$new():

  Constructor function that allocates a new struct

- \$fields:

  Named list of field metadata (offset + FFIType)

- \$get(ptr, field):

  Get field value from struct pointer

- \$set(ptr, field, value):

  Set field value in struct pointer

- \$lib:

  The compiled library object (for cleanup)

## Examples

``` r
if (FALSE) { # \dontrun{
# Define struct with field types
helpers <- ffi_create_helpers(
  "Point2D",
  list(x = ffi_int(), y = ffi_int())
)

# Create instance
pt <- helpers$new()

# Set/get fields
helpers$set(pt, "x", 42L)
helpers$set(pt, "y", 100L)
helpers$get(pt, "x")  # 42L
helpers$get(pt, "y")  # 100L

# Access field metadata
helpers$fields$x$offset  # 0
helpers$fields$y$offset  # 4
} # }
```
