# Changelog

## RSimpleFFI 1.0.1.9001 (Development)

### New Features

- **Tree-sitter parser integration**:
  - Added
    [`ffi_parse_header_ts()`](https://sounkou-bioinfo.github.io/RSimpleFFI/reference/ffi_parse_header_ts.md)
    for robust AST-based C header parsing
  - [`ffi_parse_header()`](https://sounkou-bioinfo.github.io/RSimpleFFI/reference/ffi_parse_header.md)
    now uses tree-sitter by default when available (with fallback to
    regex)
  - Correctly parses complex array declarations and nested structs
  - Depends on `treesitter` (CRAN) and `treesitter.c` packages
  - TCC preprocessor still used first to expand macros before
    tree-sitter parsing
- Struct allocation helpers are now auto-generated in R bindings:
  - `new_StructName()` - allocate and optionally initialize struct
    instances
  - `StructName_to_list()` - convert struct pointer to R list
- Typedef resolution in function wrappers:
  - Function parameters now correctly use typedef’d FFI types (e.g.,
    `SEXPTYPE` →
    [`ffi_uint()`](https://sounkou-bioinfo.github.io/RSimpleFFI/reference/ffi_uint.md))
  - FFI types shown in `@param` documentation for better IDE support
- Improved variadic function handling:
  - Functions with `...` now generate usage examples using
    [`ffi_cif_var()`](https://sounkou-bioinfo.github.io/RSimpleFFI/reference/ffi_cif_var.md)
    instead of broken wrappers

### Bug Fixes

- Fixed multi-line C comment removal in header parsing
- Fixed R name escaping for reserved words
  ([`next`](https://rdrr.io/r/base/Control.html),
  [`break`](https://rdrr.io/r/base/Control.html), etc.) and
  underscore-prefixed identifiers

## RSimpleFFI 1.0.1.9000 (Development)

### New Features

- Added `pack` parameter to
  [`ffi_struct()`](https://sounkou-bioinfo.github.io/RSimpleFFI/reference/ffi_struct.md)
  for controlling struct alignment:

  - `pack = 1` for byte-packed structs (no padding)
  - `pack = 2`, `4`, `8`, or `16` for specific alignment boundaries
  - `pack = NULL` (default) uses natural alignment
  - Matches GCC/Clang `#pragma pack(n)` and MSVC packing behavior

- Added `pack` parameter to
  [`ffi_union()`](https://sounkou-bioinfo.github.io/RSimpleFFI/reference/ffi_union.md)
  for controlling union alignment:

  - Reduces union’s alignment requirement (size stays the same)
  - Affects placement when union is a struct member
  - Example: packed union in normal struct eliminates trailing padding

- Added
  [`ffi_packed_offset()`](https://sounkou-bioinfo.github.io/RSimpleFFI/reference/ffi_packed_offset.md)
  and
  [`ffi_packed_size()`](https://sounkou-bioinfo.github.io/RSimpleFFI/reference/ffi_packed_size.md)
  for computing packed struct layouts

- Added bit-field helper functions for manual bit manipulation:

  - [`ffi_pack_bits()`](https://sounkou-bioinfo.github.io/RSimpleFFI/reference/ffi_pack_bits.md)
    /
    [`ffi_unpack_bits()`](https://sounkou-bioinfo.github.io/RSimpleFFI/reference/ffi_unpack_bits.md) -
    pack/unpack multiple bit-fields
  - [`ffi_extract_bit_field()`](https://sounkou-bioinfo.github.io/RSimpleFFI/reference/ffi_extract_bit_field.md)
    /
    [`ffi_set_bit_field()`](https://sounkou-bioinfo.github.io/RSimpleFFI/reference/ffi_set_bit_field.md) -
    single field operations
  - [`ffi_create_bitfield_accessors()`](https://sounkou-bioinfo.github.io/RSimpleFFI/reference/ffi_create_bitfield_accessors.md) -
    generate accessor functions for bit-field structs
  - 64-bit bit-field support with
    [`ffi_longlong()`](https://sounkou-bioinfo.github.io/RSimpleFFI/reference/ffi_longlong.md)
    /
    [`ffi_ulonglong()`](https://sounkou-bioinfo.github.io/RSimpleFFI/reference/ffi_ulonglong.md)
    storage types
  - Signed bit-field extraction with sign extension

- Automatic bit-field detection and code generation:

  - Parser detects bit-field syntax (`: N`) in struct definitions
  - [`generate_r_bindings()`](https://sounkou-bioinfo.github.io/RSimpleFFI/reference/generate_r_bindings.md)
    auto-generates accessor code when bit-fields are present
  - Generated code includes usage examples showing `pack()`,
    [`get()`](https://rdrr.io/r/base/get.html), `set()` operations

- Added typedef extraction from C headers:

  - [`tcc_extract_typedefs()`](https://sounkou-bioinfo.github.io/RSimpleFFI/reference/tcc_extract_typedefs.md)
    parses typedef definitions
  - [`generate_typedef_definition()`](https://sounkou-bioinfo.github.io/RSimpleFFI/reference/generate_typedef_definition.md)
    generates R code for typedefs
  - Dependency-aware ordering via
    [`sort_typedefs_by_dependency()`](https://sounkou-bioinfo.github.io/RSimpleFFI/reference/sort_typedefs_by_dependency.md)
  - Automatic detection and filtering of unresolvable typedefs

- Autogeneration of bindings using tinycc for preprocessing of header
  files

### Improvements

- Centralized C-to-FFI type mappings in
  [`get_ffi_type_map()`](https://sounkou-bioinfo.github.io/RSimpleFFI/reference/get_ffi_type_map.md):

  - 300+ type mappings including cross-platform aliases
  - Support for glibc, musl, macOS/Darwin, BSD, MSYS2/MinGW types
  - Clang-specific typedefs and compiler builtins

- Added
  [`strip_type_qualifiers()`](https://sounkou-bioinfo.github.io/RSimpleFFI/reference/strip_type_qualifiers.md)
  for handling const/volatile in type resolution

- Added
  [`get_resolvable_types()`](https://sounkou-bioinfo.github.io/RSimpleFFI/reference/get_resolvable_types.md)
  and
  [`get_c_type_keywords()`](https://sounkou-bioinfo.github.io/RSimpleFFI/reference/get_c_type_keywords.md)
  helper functions

## RSimpleFFI 1.0.1

### New Features

- Added
  [`ffi_deref_pointer()`](https://sounkou-bioinfo.github.io/RSimpleFFI/reference/ffi_deref_pointer.md)
  to dereference pointer addresses, enabling access to global pointer
  variables in shared libraries (e.g., `R_GlobalEnv`, `R_NilValue`)

- Added
  [`ffi_read_global()`](https://sounkou-bioinfo.github.io/RSimpleFFI/reference/ffi_read_global.md)
  to read typed values from global symbol addresses

- Added test global variables (`test_global_int`, `test_global_double`,
  `test_global_string`, `test_global_array`) for testing and examples

### Documentation

- Added “Dangerous: Calling R API Exported Symbols” section to README
  demonstrating how to call R’s internal C API via FFI

- Added examples showing how to access global variables and call
  `Rf_eval`, `Rf_install`, `Rf_lang1/2`, etc.

## RSimpleFFI 1.0.0

- Initial release

- Core FFI functionality using libffi

- Support for basic C types (int, double, float, pointer, string, etc.)

- Support for fixed-width integer types (int8, int16, int32, int64,
  uint8, etc.)

- Support for platform-specific types (size_t, ssize_t, long, etc.)

- Struct types with
  [`ffi_struct()`](https://sounkou-bioinfo.github.io/RSimpleFFI/reference/ffi_struct.md),
  field access, and nested structs

- Array types with
  [`ffi_array_type()`](https://sounkou-bioinfo.github.io/RSimpleFFI/reference/ffi_array_type.md)

- Variadic function support with
  [`ffi_cif_var()`](https://sounkou-bioinfo.github.io/RSimpleFFI/reference/ffi_cif_var.md)

- Closure API for R callbacks
  ([`ffi_closure()`](https://sounkou-bioinfo.github.io/RSimpleFFI/reference/ffi_closure.md))

- Dynamic library loading with
  [`dll_load()`](https://sounkou-bioinfo.github.io/RSimpleFFI/reference/dynamic_library_management.md),
  [`dll_compile_and_load()`](https://sounkou-bioinfo.github.io/RSimpleFFI/reference/dynamic_library_management.md)

- Memory allocation with
  [`ffi_alloc()`](https://sounkou-bioinfo.github.io/RSimpleFFI/reference/alloc.md),
  [`ffi_copy_array()`](https://sounkou-bioinfo.github.io/RSimpleFFI/reference/ffi_copy_array.md),
  [`ffi_fill_typed_buffer()`](https://sounkou-bioinfo.github.io/RSimpleFFI/reference/ffi_fill_typed_buffer.md)
