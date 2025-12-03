# Get the master C-to-FFI type mapping

Returns a named character vector mapping C type names to FFI type
constructor calls. This is the single source of truth for type mappings
used across all code generation functions (struct, union, typedef,
function wrappers).

## Usage

``` r
get_ffi_type_map()
```

## Value

Named character vector: names are C types, values are FFI constructor
calls

## Details

Includes:

- Standard C types (char, int, long, etc.)

- Fixed-width types (int8_t, uint32_t, etc.)

- POSIX types (size_t, ssize_t, off_t, pid_t, etc.)

- glibc/musl internal types (\_\_ssize_t, \_\_pid_t, etc.)

- macOS/Darwin types (\_\_darwin_size_t, etc.)

- BSD types (u_int32_t, register_t, etc.)

- Windows/MSYS2/MinGW types (DWORD, HANDLE, \_\_int64, etc.)

- Clang/LLVM builtin types (**SIZE_TYPE**, etc.)
