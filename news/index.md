# Changelog

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
