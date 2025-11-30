
# RSimpleFFI

<!-- badges: start -->

[![R-CMD-check](https://github.com/sounkou-bioinfo/RSimpleFFI/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/sounkou-bioinfo/RSimpleFFI/actions/workflows/R-CMD-check.yaml)[![RSimpleFFI
status
badge](https://sounkou-bioinfo.r-universe.dev/RSimpleFFI/badges/version)](https://sounkou-bioinfo.r-universe.dev/RSimpleFFI)
<!-- badges: end -->

A Simple Foreign Function Interface (FFI) for R using libffi.

## Overview

RSimpleFFI lets you call C functions from R using the
[libffi](https://github.com/libffi/libffi) library. It supports several
C types including integers, floats, and platform-specific types. It only
needs libffi which is available on most systems and is vendored in this
package for unix systems. RSimpleFFI is inspired by the [Rffi
package](https://github.com/omegahat/Rffi/) by Duncan Temple Lang. It
builds on the same structure with S7 classes.

## Installation

You can install RSimpleFFI from source using the `remotes` package or
via r-universe

``` r
# r-universe

install.packages('RSimpleFFI', repos = c('https://sounkou-bioinfo.r-universe.dev', 'https://cloud.r-project.org'))

# remotes

remotes::install_git("sounkou-bioinfo/RSimpleFFI")
```

on windows, it requires libffi to be installed along with pkg-config :
this is always the case with recent
[RTools](https://cran.r-project.org/bin/windows/Rtools/rtools45/news.html).
On Unix-alikes libffi is always built from source.

## Quick Start

``` r
library(RSimpleFFI)
# lib ffi version
libffi_version()
#> [1] "3.5.2"
# Create FFI types
int_type <- ffi_int()
double_type <- ffi_double()

# Get a C function symbol (using built-in test function compiled with the package)
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
platform-specific types

### Basic Types

``` r
void_type <- ffi_void()
int_type <- ffi_int() 
double_type <- ffi_double()
float_type <- ffi_float()
pointer_type <- ffi_pointer()
string_type <- ffi_string()

int8_type <- ffi_int8()
uint8_type <- ffi_uint8()
int16_type <- ffi_int16()
uint16_type <- ffi_uint16()
int32_type <- ffi_int32() 
uint32_type <- ffi_uint32()
int64_type <- ffi_int64()
uint64_type <- ffi_uint64()

size_t_type <- ffi_size_t()
ssize_t_type <- ffi_ssize_t()
long_type <- ffi_long()
ulong_type <- ffi_ulong()

longdouble_type <- ffi_longdouble()
bool_type <- ffi_bool()
wchar_type <- ffi_wchar_t()
```

### Typed Buffers

We can allocate typed buffers using `ffi_alloc()` and read/write data
using `ffi_copy_array()` and `ffi_fill_typed_buffer()`

``` r
# Allocate a buffer for 10 integers
int_type <- ffi_int()
int_buf <- ffi_alloc(int_type, 10L)

# Read back as R vector (using ffi_copy_array)
ffi_copy_array(int_buf, 10L, int_type)
#>  [1] 0 0 0 0 0 0 0 0 0 0

# You can use ffi_alloc for any builtin type:
double_type <- ffi_double()
double_buf <- ffi_alloc(double_type, 5L)
ffi_copy_array(double_buf, 5L, double_type)
#> [1] 0 0 0 0 0
```

### Array Types (ArrayType)

Array types can be created using `ffi_array_type()` and used with
`ffi_alloc()` and `ffi_copy_array_type()`. They provide a convenient way
to handle fixed-size arrays and passed them to C functions via pointers.

``` r
# Allocate an array of 4 integers
int_type <- ffi_int()
arr_type <- ffi_array_type(int_type, 4L)
arr_ptr <- ffi_alloc(arr_type)

# Write values into the buffer using ffi_fill_typed_buffer
vals <- as.integer(c(10L, 20L, 30L, 40L))
ffi_fill_typed_buffer(arr_ptr, vals, int_type)
#> NULL

# Read back as R vector
result <- ffi_copy_array_type(arr_ptr, arr_type)
result
#> [1] 10 20 30 40
```

### Struct Types

You can define and use C struct types using `ffi_struct()`,
`ffi_alloc()`, `ffi_get_field()`, and `ffi_set_field()`

``` r

# Define a struct type: struct Point { int x; double y; }
point_type <- ffi_struct(x = ffi_int(), y = ffi_double())

point_type
#> StructType(fields=[x, y], size=16)
#> Fields:
#>   x: FFIType(int, size=4)
#>   y: FFIType(double, size=8)

# Allocate a struct instance
point_ptr <- ffi_alloc(point_type)

# Set fields
ffi_set_field(point_ptr, "x", 42L, point_type)
ffi_set_field(point_ptr, "y", 3.14, point_type)

# Get fields
x_val <- ffi_get_field(point_ptr, "x", point_type)
y_val <- ffi_get_field(point_ptr, "y", point_type)
x_val 
#> [1] 42
y_val 
#> [1] 3.14
# You can also use integer field indices (1-based):
ffi_set_field(point_ptr, 1L, 100L, point_type)  
ffi_get_field(point_ptr, 1L, point_type)       
#> [1] 100
```

You can define more complex structs by adding more fields and using any
supported FFI type.

## Function Calling

### Basic Function Calls

The package comes with some built-in C test functions for testing, they
are defined in [src/test_functions.c](src/test_functions.c)

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

#### Testing Integer Types

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

#### Floating-Point Types

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

#### String output example

This calls a C function that returns a string (const char\*) as pointer

``` r
string_func <- ffi_symbol("test_return_string")
string_cif <- ffi_cif(string_type)
string_result <- ffi_call(string_cif, string_func)
string_result
#> <pointer: 0x74d118b26d50>
pointer_to_string(string_result)
#> [1] "Hello from C!"
```

#### Struct Types

``` r
# Define struct type: struct Point { int x; double y; }
point_type <- ffi_struct(x = ffi_int(), y = ffi_double())

# Allocate and set fields
point_ptr <- ffi_alloc(point_type)
ffi_set_field(point_ptr, "x", 42L, point_type)
ffi_set_field(point_ptr, "y", 3.14, point_type)

# Call the built-in C function: int test_get_point_x(Point2D* point)
get_point_x_func <- ffi_symbol("test_get_point_x")
get_point_x_cif <- ffi_cif(ffi_int(), ffi_pointer())
result_x <- ffi_call(get_point_x_cif, get_point_x_func, point_ptr)
result_x  
#> [1] 42
```

#### Type Conversions

RSimpleFFI attemps to convert between R and C types automatically

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

### Call system libraries functions or external shared libraries

You can load external shared libraries at runtime using RSimpleFFI’s
`dll_load()` and `dll_ffi_symbol()` functions. These are wrappers around
R’s native `dyn.load()` facilities that search for shared libraries in
system paths when required.

#### Search so files in system paths

``` r
# Example: call the C standard library rand() function
libc_path <- dll_load_system("libc.so.6")
#> Loading system library from: /usr/lib/x86_64-linux-gnu/libc.so.6
rand_func <- dll_ffi_symbol("rand", ffi_int())
rand_value <- rand_func()
rand_value
#> [1] 1737417453
rand_value <- rand_func()
rand_value
#> [1] 243508900
dll_unload(libc_path)
```

#### Explicitly load shared libraries

``` r
# Find the libc shared object (libc is always present)
so_files <- list.files("/lib/x86_64-linux-gnu", pattern = "^libc[.]so[.]6$", full.names = TRUE)
if (length(so_files) == 0) stop("libc.so.6 not found")
lib_path <- dll_load(so_files[1])

# Allocate a buffer of 8 bytes
raw_type <- ffi_raw()
buf_ptr <- ffi_alloc(raw_type, 8L)
rawToChar(ffi_copy_array(buf_ptr, 8L, raw_type))
#> [1] ""
# Get memset from libc: void *memset(void *s, int c, size_t n)
memset_fn <- dll_ffi_symbol("memset", ffi_pointer(), ffi_pointer(), ffi_int(), ffi_size_t())

# Fill the buffer with ASCII 'A' (0x41)
memset_fn(buf_ptr, as.integer(0x41), 8L)
#> <pointer: 0x5a409ba3fd00>

# Read back the buffer and print as string
rawToChar(ffi_copy_array(buf_ptr, 8L, raw_type))
#> [1] "AAAAAAAA"

pointer_to_string(buf_ptr)
#> [1] "AAAAAAAA"

dll_unload(lib_path)
```

### Compile and Load C Code

The package provides facilities to load C code on the fly. The compiler
uses R CMD SHLIB under the hood

``` r
c_code <- '
int add_numbers(int a, int b) {
    return a + b;
}
'

lib_path <- dll_compile_and_load(c_code, "example_lib")
int_t <- ffi_int()
add_fn <- dll_ffi_symbol("add_numbers", int_t, int_t, int_t)
result <- add_fn(10L, 5L)
result
#> [1] 15

dll_unload(lib_path)
```

``` r
math_code <- '
#include <math.h>
double compute_distance(double x1, double y1, double x2, double y2) {
    double dx = x2 - x1;
    double dy = y2 - y1;
    return sqrt(dx*dx + dy*dy);
}
'

lib_path <- dll_compile_and_load(math_code, "math_lib", libs = "m")
double_t <- ffi_double()
distance_fn <- dll_ffi_symbol("compute_distance", double_t, double_t, double_t, double_t, double_t)
dist <- distance_fn(0.0, 0.0, 3.0, 4.0)
dist
#> [1] 5

dll_unload(lib_path)
```

## Benchmarking

We run some benchmarks to estimate the performance of FFI calls (i.e the
overhead of calling C functions from R using RSimpleFFI) compared to
native R C built-in functions.

### R builtin C functions

``` r

set.seed(123)
n <- 10000
x_vec <- runif(n, 1, 100)
x_ptr <- ffi_alloc(ffi_double(), n)
ffi_fill_typed_buffer(x_ptr, x_vec, ffi_double())
#> NULL
out_ptr <- ffi_alloc(ffi_double(), n)

math_code <- '
#include <math.h>
void vec_sqrt(const double* x, double* out, int n) {
    for (int i = 0; i < n; ++i) out[i] = sqrt(x[i]);
}
'
lib_path <- dll_compile_and_load(math_code, "bench_vec", libs = "m", cflags = "-O3")
vec_sqrt_func <- dll_ffi_symbol("vec_sqrt", ffi_void(), ffi_pointer(), ffi_pointer(), ffi_int())

benchmark_result <- bench::mark(
  native_r = sqrt(x_vec),
  ffi_call = { vec_sqrt_func(x_ptr, out_ptr, n); ffi_copy_array(out_ptr, n, ffi_double()) },
  check = FALSE,
  iterations = 100
)
benchmark_result
#> # A tibble: 2 × 6
#>   expression      min   median `itr/sec` mem_alloc `gc/sec`
#>   <bch:expr> <bch:tm> <bch:tm>     <dbl> <bch:byt>    <dbl>
#> 1 native_r       13µs   28.9µs    34669.    78.2KB        0
#> 2 ffi_call     94.9µs   99.9µs     9836.    78.7KB        0
dll_unload(lib_path)
```

### Interpreted R Code

We compare a pure C convolution (via FFI) to a simple R implementation.

``` r
# Slow R convolution
slow_convolve <- function(a, b) {
  ab <- double(length(a) + length(b) - 1)
  for (i in seq_along(a)) {
    for (j in seq_along(b)) {
      ab[i + j - 1] <- ab[i + j - 1] + a[i] * b[j]
    }
  }
  ab
}

# C code for convolution (matches R)
conv_code <- '
#include <stdio.h>
void c_convolve(const double* signal, int n_signal, const double* kernel, int n_kernel, double* out) {
  int n_out = n_signal + n_kernel - 1;
  for (int i = 0; i < n_out; ++i) {
    out[i] = 0.0;
    for (int j = 0; j < n_kernel; ++j) {
      int k = i - j;
      if (k >= 0 && k < n_signal) {
        //printf("signal[%d]=%f, kernel[%d]=%f\\n", k, signal[k], j, kernel[j]);
        out[i] += signal[k] * kernel[j];
      }
    }
  }
}
'

set.seed(1995)
signal <- rnorm(10000)
kernel <- c(0.2, 0.5, 0.3)
n_signal <- length(signal)
n_kernel <- length(kernel)
n_out <- n_signal + n_kernel - 1

# Allocate buffers for FFI
signal_ptr <- ffi_alloc(ffi_double(), n_signal)
kernel_ptr <- ffi_alloc(ffi_double(), n_kernel)
out_ptr <- ffi_alloc(ffi_double(), n_out)

# Fill buffers
ffi_fill_typed_buffer(signal_ptr, signal, ffi_double())
#> NULL
all.equal(ffi_copy_array(signal_ptr, length(signal),
ffi_double()), signal)
#> [1] TRUE
ffi_fill_typed_buffer(kernel_ptr, kernel, ffi_double())
#> NULL
all.equal(ffi_copy_array(kernel_ptr, length(kernel),
ffi_double()), kernel)
#> [1] TRUE

# Compile and load C convolution
lib_path <- dll_compile_and_load(conv_code, "bench_conv.so", cflags = "-O3")
pointer_t <- ffi_pointer()
c_conv_fn <- dll_ffi_symbol(
  "c_convolve",
  ffi_void(),
  pointer_t, ffi_int(),
  pointer_t, ffi_int(),
  pointer_t
)

# Run C convolution via FFI
c_conv_fn(
      signal_ptr, 
      as.integer(n_signal),
      kernel_ptr,
      as.integer(n_kernel),
      out_ptr)
#> NULL
out_ptr
#> <pointer: 0x5a40a09fa6e0>
c_result <- ffi_copy_array(out_ptr, n_out, ffi_double())

# Run R convolution
r_result <- slow_convolve(signal, kernel)

# Check results
all.equal(ffi_copy_array(signal_ptr, length(signal),
ffi_double()), signal)
#> [1] TRUE
all.equal(ffi_copy_array(kernel_ptr, length(kernel),
ffi_double()), kernel)
#> [1] TRUE
all.equal(as.numeric(c_result), as.numeric(r_result))
#> [1] TRUE

ffi_copy_array(out_ptr, n_out, ffi_double()) |> head()
#> [1] 0.21215265 0.46328129 0.17953895 0.05081738 0.57361490 0.88130298
# Benchmark
benchmark_result <- bench::mark(
  r = slow_convolve(signal, kernel),
  c_ffi = {
    c_conv_fn(signal_ptr, as.integer(n_signal), kernel_ptr, as.integer(n_kernel), out_ptr)
    ffi_copy_array(out_ptr, n_out, ffi_double())
  },
  check = FALSE,
  iterations = 20
)
benchmark_result
#> # A tibble: 2 × 6
#>   expression      min   median `itr/sec` mem_alloc `gc/sec`
#>   <bch:expr> <bch:tm> <bch:tm>     <dbl> <bch:byt>    <dbl>
#> 1 r            2.46ms   2.67ms      368.    78.2KB     19.4
#> 2 c_ffi       99.58µs  119.1µs     8107.    78.7KB      0

dll_unload(lib_path)
```

## Limitations and issues

Right now, there are unimplemented features and limitations, including
unnecessary copying, lack of protection, and several potential memory
leaks. The interface can and should be refined further. Our type objects
are C pointers (to static variables for basic types) that are never
finalized. Additionally, the type coercions are not C99-conformant, and
we do not support complex types. The more “dynamic” calling interface
via the closure API of libffi is not included. We do not provide any
facility to create interfaces from C files like the
[RGCCTranslationUnit](https://github.com/omegahat/RGCCTranslationUnit).

## License

This project is licensed under the GPL-3 License.

# References

- [Rffi](https://github.com/omegahat/Rffi)

- [libffi
  Examples](http://www.chiark.greenend.org.uk/doc/libffi-dev/html/Using-libffi.html)

- [CPython’s ctypes
  module](https://docs.python.org/3/library/ctypes.html)
