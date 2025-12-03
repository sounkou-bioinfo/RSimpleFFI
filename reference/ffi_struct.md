# Create FFI structure type

Creates an FFI structure type from named field types. By default, libffi
uses natural alignment (each field aligned to its size). Use the `pack`
parameter to specify tighter packing similar to `#pragma pack(n)` in C.

## Usage

``` r
ffi_struct(..., pack = NULL)
```

## Arguments

- ...:

  Named FFIType objects representing struct fields

- pack:

  Integer specifying packing alignment (1, 2, 4, 8, or 16), or NULL for
  default/natural alignment. When pack=1, fields are byte-aligned (no
  padding).

## Value

StructType object

## Packing and libffi

The `pack` parameter affects how
[`ffi_offsetof()`](https://sounkou-bioinfo.github.io/RSimpleFFI/reference/ffi_offsetof.md),
[`ffi_sizeof()`](https://sounkou-bioinfo.github.io/RSimpleFFI/reference/ffi_sizeof.md),
[`ffi_get_field()`](https://sounkou-bioinfo.github.io/RSimpleFFI/reference/ffi_get_field.md),
and
[`ffi_set_field()`](https://sounkou-bioinfo.github.io/RSimpleFFI/reference/ffi_set_field.md)
calculate field offsets and struct size. This is useful when working
with memory buffers that use packed C structs.

**Important**: libffi internally always uses natural alignment when
passing structs by value in function calls. Packed structs work
correctly when:

- Reading/writing to memory buffers (ffi_alloc, ffi_get_field,
  ffi_set_field)

- Passing struct pointers to C functions (the C code handles the packed
  layout)

- Computing offsets for manual memory manipulation

However, passing a packed struct **by value** to ffi_call may not work
correctly because libffi will use natural alignment for the call.

## Examples

``` r
# Natural alignment (default)
Point <- ffi_struct(x = ffi_int(), y = ffi_int())

# Packed struct (1-byte alignment)
PackedData <- ffi_struct(
  flag = ffi_uint8(),
  value = ffi_int32(),
  pack = 1
)

# Check sizes
ffi_sizeof(Point) # Natural size
#> [1] 8
ffi_sizeof(PackedData) # Packed size (smaller)
#> [1] 5
```
