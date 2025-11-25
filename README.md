
# RSimpleFFI

<!-- badges: start -->

[![R-CMD-check](https://github.com/sounkou-bioinfo/RSimpleFFI/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/sounkou-bioinfo/RSimpleFFI/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

A Foreign Function Interface (FFI) for R using libffi. Call C functions
from R with S7 classes.

## Overview

RSimpleFFI lets you call C functions from R using the libffi library. It
supports over 25 different C types including integers, floats, and
platform-specific types. The package uses S7 classes for type safety and
handles memory management automatically. It only needs libffi which is
available on most systems.

## Related Projects

RSimpleFFI is inspired by the [Rffi
package](https://github.com/omegahat/Rffi/) by Duncan Temple Langs.
RSimpleFFI builds on these concepts with S7 classes.

## Installation

You can install RSimpleFFI from source:

``` r
remotes::install_git("sounkou-bioinfo/RSimpleFFI")
```

### System Requirements

RSimpleFFI requires libffi to be installed on your system:

``` bash
# Ubuntu/Debian
sudo apt-get install libffi-dev

# macOS (via Homebrew)
brew install libffi

# CentOS/RHEL/Fedora
sudo yum install libffi-devel  # or dnf install libffi-devel
```

## Quick Start

``` r
library(RSimpleFFI)

# Create FFI types
int_type <- ffi_int()
double_type <- ffi_double()

# Get a C function symbol (using built-in test function)
add_func <- ffi_symbol("test_add_int")

# Create call interface (CIF)
cif <- ffi_cif(int_type, int_type, int_type)  # return int, takes two ints

# Call the function
result <- ffi_call(cif, add_func, 15L, 27L)
result
#> [1] 42
```

## Type System

RSimpleFFI supports many C types including integers, floats, and
platform-specific types:

### Basic Types

``` r
void_type <- ffi_void()
int_type <- ffi_int() 
double_type <- ffi_double()
float_type <- ffi_float()
pointer_type <- ffi_pointer()
string_type <- ffi_string()
```

### Extended Integer Types

``` r
int8_type <- ffi_int8()
uint8_type <- ffi_uint8()
int16_type <- ffi_int16()
uint16_type <- ffi_uint16()
int32_type <- ffi_int32() 
uint32_type <- ffi_uint32()
int64_type <- ffi_int64()
uint64_type <- ffi_uint64()
```

### Platform-Dependent Types

``` r
size_t_type <- ffi_size_t()
ssize_t_type <- ffi_ssize_t()
long_type <- ffi_long()
ulong_type <- ffi_ulong()
```

### Extended Floating-Point and Special Types

``` r
longdouble_type <- ffi_longdouble()
bool_type <- ffi_bool()
wchar_type <- ffi_wchar_t()
```

## Function Call Examples

### Integer

``` r
# test_int8_func: returns input + 1
int8_func <- ffi_symbol("test_int8_func")
int8_cif <- ffi_cif(int8_type, int8_type)
int8_result <- ffi_call(int8_cif, int8_func, 42L)
int8_result  # 42 + 1 = 43
#> [1] 43

# test_uint32_func: returns input * 3
uint32_func <- ffi_symbol("test_uint32_func") 
uint32_cif <- ffi_cif(uint32_type, uint32_type)
uint32_result <- ffi_call(uint32_cif, uint32_func, 123L)
uint32_result  # 123 * 3 = 369
#> [1] 369

# test_int64_func: returns input * 4
int64_func <- ffi_symbol("test_int64_func")
int64_cif <- ffi_cif(int64_type, int64_type)
int64_result <- ffi_call(int64_cif, int64_func, 999L)
int64_result  # 999 * 4 = 3996
#> [1] 3996
```

### Floating-Point Types

``` r
add_func <- ffi_symbol("test_add_int")
add_cif <- ffi_cif(int_type, int_type, int_type)
int_result <- ffi_call(add_cif, add_func, 10L, 5L)
int_result
#> [1] 15

double_func <- ffi_symbol("test_add_double")
double_cif <- ffi_cif(double_type, double_type, double_type)
double_result <- ffi_call(double_cif, double_func, 3.14, 2.86)
double_result
#> [1] 6

float_func <- ffi_symbol("test_add_float")
float_cif <- ffi_cif(float_type, float_type, float_type)
float_result <- ffi_call(float_cif, float_func, 1.5, 2.5)
float_result
#> [1] 4
```

### Other calls

``` r
void_func <- ffi_symbol("test_void_function")
void_cif <- ffi_cif(void_type)
ffi_call(void_cif, void_func)
#> NULL

factorial_func <- ffi_symbol("test_factorial")
factorial_cif <- ffi_cif(int_type, int_type)
factorial_result <- ffi_call(factorial_cif, factorial_func, 5L)
factorial_result
#> [1] 120
```

## Type Conversions

RSimpleFFI converts between R and C types automatically:

``` r
int16_result <- ffi_call(ffi_cif(int16_type, int16_type), 
                        ffi_symbol("test_int16_func"), 1000L)
int16_result
#> [1] 2000

uint16_result <- ffi_call(ffi_cif(uint16_type, uint16_type),
                         ffi_symbol("test_uint16_func"), 2000.0)
uint16_result
#> [1] 4000

bool_result <- ffi_call(ffi_cif(bool_type, bool_type),
                       ffi_symbol("test_bool_func"), TRUE)
bool_result
#> [1] 0
```

## Buffer and Pointer Management

``` r
# Allocate typed buffer
ptr <- ffi_alloc(ffi_double(), 5L)
ptr
#> <pointer: 0x5b41db641d60>

# Allocate raw buffer
raw_ptr <- ffi_alloc_buffer(32L)
raw_ptr
#> <pointer: 0x5b41d89b7b40>

# Check for null pointer
is_null_pointer(ptr)
#> [1] FALSE

# Copy array from native memory
ffi_copy_array(ptr, 5L, ffi_double())
#> [1] 0 0 0 0 0
```

## Structs and Complex Types

``` r
Point <- ffi_struct(x = ffi_int(), y = ffi_int())
Point
```

``` r
pt_ptr <- ffi_alloc(Point)
ffi_set_field(pt_ptr, "x", 10L, Point)
ffi_get_field(pt_ptr, "x", Point) 
ffi_get_field
```

### Structs Example

``` r
# Struct compilation example (struct output via pointer)
struct_code <- '
typedef struct { int x, y; } Point;
void create_point(int x, int y, Point* out) {
    out->x = x;
    out->y = y;
}
'
lib_handle <- dll_compile_and_load(struct_code, "struct_test")

Point_t <- ffi_struct(x = ffi_int(), y = ffi_int())
create_point <- dll_ffi_symbol("create_point", ffi_void(), ffi_int(), ffi_int(), ffi_pointer())
pt <- ffi_alloc(Point_t)
create_point(10L, 20L, pt)
#> NULL
ffi_get_field(pt, "x", Point_t)  # should return 10
#> [1] 10
ffi_get_field(pt, "y", Point_t)  # should return 20
#> [1] 20

dll_unload(lib_handle)
```

## Advanced Usage

### Custom Type Definitions

``` r
ptr_func <- ffi_symbol("test_return_pointer")
ptr_cif <- ffi_cif(pointer_type, pointer_type)
test_ptr <- NULL
ptr_result <- ffi_call(ptr_cif, ptr_func, test_ptr)
ptr_result
#> NULL
```

### Error Handling

RSimpleFFI provides clear error messages for common issues:

``` r
tryCatch({
  invalid_func <- ffi_symbol("nonexistent_function")
}, error = function(e) {
  e$message
})
#> [1] "Symbol not found: nonexistent_function. Make sure the library containing this symbol is loaded."

tryCatch({
  test_cif <- ffi_cif(int_type, int_type, int_type)
  bad_type <- ffi_call(test_cif, add_func, "not_a_number", 5L)
}, error = function(e) {
  e$message
})
#> [1] "Cannot convert to int"
```

## Dynamic Library Loading

RSimpleFFI supports loading external shared libraries at runtime using
R’s native `dyn.load()` facilities. This allows you to compile and use
your own C functions dynamically:

``` r
c_code <- '
int add_numbers(int a, int b) {
    return a + b;
}
'

lib_handle <- dll_compile_and_load(c_code, "example_lib")
int_t <- ffi_int()
add_fn <- dll_ffi_symbol("add_numbers", int_t, int_t, int_t)
result <- add_fn(10L, 5L)
result
#> [1] 15

dll_unload(lib_handle)
```

## System Library Access

``` r
tryCatch({
  libc_handle <- dll_load_system("c")
  if (!is.null(libc_handle)) "System library access works"
}, error = function(e) e$message)
#> [1] "System library access works"
```

### libc rand

``` r
# Example: call the C standard library rand() function
libc_handle <- dll_load_system("c")
rand_func <- dll_ffi_symbol("rand", ffi_int())
rand_value <- rand_func()
rand_value
#> [1] 483920519
#> [1] 1261047682

dll_unload(libc_handle)
```

### libcrypto

``` r
# Example: loading libcrypto and calling AES_encrypt using pointers
so_files <- list.files("/usr/lib/x86_64-linux-gnu", pattern = "^libcrypto[.]so.3", full.names = TRUE)
so_files
#> [1] "/usr/lib/x86_64-linux-gnu/libcrypto.so.3"
#> [1] "/usr/lib/x86_64-linux-gnu/libcrypto.so.3"
lib_handle <- dll_load(so_files[1])
lib_handle
#> [1] "/usr/lib/x86_64-linux-gnu/libcrypto.so.3"
#> [1] "/usr/lib/x86_64-linux-gnu/libcrypto.so.3"

# Allocate memory for input and output buffers (16 bytes each) using RSimpleFFI's typed allocator
raw_type <- ffi_raw()
inbuf_ptr <- ffi_alloc(raw_type, 16L)
outbuf_ptr <- ffi_alloc(raw_type, 16L)

# Use a character string as a dummy pointer for the key argument (for demonstration only)
fake_key <- "dummy_key_ptr"
# Use ffi_pointer() for all pointer arguments
aes_encrypt_fn <- dll_ffi_symbol("AES_encrypt", ffi_void(), ffi_pointer(), ffi_pointer(), ffi_pointer())
aes_encrypt_fn(inbuf_ptr, outbuf_ptr, fake_key)
#> NULL
#> NULL
ffi_copy_array(outbuf_ptr, 16L, raw_type)
#>  [1] 6d 08 b3 31 d1 26 76 f9 c8 a9 3f ee 14 d3 e7 ba
#>  [1] 0a ae 48 58 58 f0 c0 2c ca cb 6f 54 82 97 7d e9
dll_unload(lib_handle)
```

### Advanced DLL Compilation

The compiler uses R’s configured toolchain for maximum compatibility:

``` r
math_code <- '
#include <math.h>
double compute_distance(double x1, double y1, double x2, double y2) {
    double dx = x2 - x1;
    double dy = y2 - y1;
    return sqrt(dx*dx + dy*dy);
}
'

lib_handle <- dll_compile_and_load(math_code, "math_lib", libs = "m")
double_t <- ffi_double()
distance_fn <- dll_ffi_symbol("compute_distance", double_t, double_t, double_t, double_t, double_t)
dist <- distance_fn(0.0, 0.0, 3.0, 4.0)
dist
#> [1] 5

dll_unload(lib_handle)
```

## License

This project is licensed under the GPL-3 License - see the
[LICENSE](LICENSE) file for details.
