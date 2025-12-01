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
