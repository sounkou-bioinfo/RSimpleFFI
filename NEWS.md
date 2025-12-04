# RSimpleFFI 1.0.1.9001 (Development)

## New Features

* **Tree-sitter parser integration**:
  - Added `ffi_parse_header_ts()` for robust AST-based C header parsing
  - `ffi_parse_header()` now uses tree-sitter by default when available (with fallback to regex)
  - Correctly parses complex array declarations and nested structs
  - **Now detects `__attribute__((packed))` and sets `packed` attribute on structs/unions**
  - Generates code with `pack=1` for packed structs automatically
  - Depends on `treesitter` (CRAN) and `treesitter.c` packages
  - TCC preprocessor still used first to expand macros before tree-sitter parsing

* Struct allocation helpers are now auto-generated in R bindings:
  - `new_StructName()` - allocate and optionally initialize struct instances
  - `StructName_to_list()` - convert struct pointer to R list

* Typedef resolution in function wrappers:
  - Function parameters now correctly use typedef'd FFI types (e.g., `SEXPTYPE` → `ffi_uint()`)
  - FFI types shown in `@param` documentation for better IDE support

* Improved variadic function handling:
  - Functions with `...` now generate usage examples using `ffi_cif_var()` instead of broken wrappers

## Bug Fixes
 
* Fixed multi-line C comment removal in header parsing
* Fixed R name escaping for reserved words (`next`, `break`, etc.) and underscore-prefixed identifiers

# RSimpleFFI 1.0.1.9000 (Development)

## New Features

* Added `pack` parameter to `ffi_struct()` for controlling struct alignment:
  - `pack = 1` for byte-packed structs (no padding)
  - `pack = 2`, `4`, `8`, or `16` for specific alignment boundaries
  - `pack = NULL` (default) uses natural alignment
  - Matches GCC/Clang `#pragma pack(n)` and MSVC packing behavior

* Added `pack` parameter to `ffi_union()` for controlling union alignment:
  - Reduces union's alignment requirement (size stays the same)
  - Affects placement when union is a struct member
  - Example: packed union in normal struct eliminates trailing padding

* Added `ffi_packed_offset()` and `ffi_packed_size()` for computing packed struct layouts

* Added bit-field helper functions for manual bit manipulation:
  - `ffi_pack_bits()` / `ffi_unpack_bits()` - pack/unpack multiple bit-fields
  - `ffi_extract_bit_field()` / `ffi_set_bit_field()` - single field operations
  - `ffi_create_bitfield_accessors()` - generate accessor functions for bit-field structs
  - 64-bit bit-field support with `ffi_longlong()` / `ffi_ulonglong()` storage types
  - Signed bit-field extraction with sign extension

* Automatic bit-field detection and code generation:
  - Parser detects bit-field syntax (`: N`) in struct definitions
  - `generate_r_bindings()` auto-generates accessor code when bit-fields are present
  - Generated code includes usage examples showing `pack()`, `get()`, `set()` operations

* Added typedef extraction from C headers:
  - `tcc_extract_typedefs()` parses typedef definitions
  - `generate_typedef_definition()` generates R code for typedefs
  - Dependency-aware ordering via `sort_typedefs_by_dependency()`
  - Automatic detection and filtering of unresolvable typedefs

* Autogeneration of bindings using tinycc for preprocessing of header files

## Improvements

* Centralized C-to-FFI type mappings in `get_ffi_type_map()`:
  - 300+ type mappings including cross-platform aliases
  - Support for glibc, musl, macOS/Darwin, BSD, MSYS2/MinGW types
  - Clang-specific typedefs and compiler builtins

* Added `strip_type_qualifiers()` for handling const/volatile in type resolution

* Added `get_resolvable_types()` and `get_c_type_keywords()` helper functions


# RSimpleFFI 1.0.1

## New Features

* Added `ffi_deref_pointer()` to dereference pointer addresses, enabling access to global pointer variables in shared libraries (e.g., `R_GlobalEnv`, `R_NilValue`)

* Added `ffi_read_global()` to read typed values from global symbol addresses

* Added test global variables (`test_global_int`, `test_global_double`, `test_global_string`, `test_global_array`) for testing and examples

## Documentation

* Added "Dangerous: Calling R API Exported Symbols" section to README demonstrating how to call R's internal C API via FFI

* Added examples showing how to access global variables and call `Rf_eval`, `Rf_install`, `Rf_lang1/2`, etc.

# RSimpleFFI 1.0.0

* Initial  release

* Core FFI functionality using libffi

* Support for basic C types (int, double, float, pointer, string, etc.)

* Support for fixed-width integer types (int8, int16, int32, int64, uint8, etc.)

* Support for platform-specific types (size_t, ssize_t, long, etc.)

* Struct types with `ffi_struct()`, field access, and nested structs

* Array types with `ffi_array_type()`

* Variadic function support with `ffi_cif_var()`

* Closure API for R callbacks (`ffi_closure()`)

* Dynamic library loading with `dll_load()`, `dll_compile_and_load()`

* Memory allocation with `ffi_alloc()`, `ffi_copy_array()`, `ffi_fill_typed_buffer()`
