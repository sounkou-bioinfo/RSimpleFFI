
# RSimpleFFI

<!-- badges: start -->

[![R-CMD-check](https://github.com/sounkou-bioinfo/RSimpleFFI/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/sounkou-bioinfo/RSimpleFFI/actions/workflows/R-CMD-check.yaml)[![RSimpleFFI
status
badge](https://sounkou-bioinfo.r-universe.dev/RSimpleFFI/badges/version)](https://sounkou-bioinfo.r-universe.dev/RSimpleFFI)[![R-hub](https://github.com/sounkou-bioinfo/RSimpleFFI/actions/workflows/rhub.yaml/badge.svg)](https://github.com/sounkou-bioinfo/RSimpleFFI/actions/workflows/rhub.yaml)
<!-- badges: end -->

A Simple Foreign Function Interface (FFI) for R using libffi.

## Abstract

RSimpleFFI lets you call C functions from R using the
[libffi](https://github.com/libffi/libffi) library. It supports several
C types including integers, floats, and platform-specific types. It
requires libffi which is available on most R supported plateforms and is
vendored in this package for unix systems. It is inspired by the [Rffi
package](https://github.com/omegahat/Rffi/) by Duncan Temple Lang. We
build on the same structure with S7 classes.

As an experimental feature, the package includes automatic R binding
generation from C header files. We use the
[`tinycc`](https://github.com/tinycc/tinycc) compiler cli for
pre-processed C file generation, allowing (attempt) to parse C headers
and automatically generate R wrapper functions for easy package/quick
script development using
[`treessiter.c`](https://github.com/sounkou-bioinfo/treesitter.c)
package. `tinycc` is not used for the in memory compilation facilities
but to preprocess the headers given includes and maybe in the future for
JIT.

## Prior Art

Of course this package is inspired by
[Rffi](https://github.com/omegahat/Rffi).
[`Dyncall`](https://dyncall.org/docs/dynload.3.html) is an alternative C
library for dynamic ffi calls like `libffi`. The CRAN package
[rdyncall](https://cran.r-project.org/web/packages/rdyncall/index.html)
was archived, but there is ongoing project at
[hongyuanjia/rdyncall](https://github.com/hongyuanjia/rdyncall) to get
it back on CRAN.

## Installation

You can install RSimpleFFI from source using the `remotes` package or
via r-universe. On windows, it requires libffi to be installed along
with pkg-config : this is always the case with recent
[RTools](https://cran.r-project.org/bin/windows/Rtools/rtools45/news.html).
On Unix-alikes libffi is always built from source.

``` r
# r-universe
install.packages('RSimpleFFI', repos = c('https://sounkou-bioinfo.r-universe.dev', 'https://cloud.r-project.org'))
# remotes
remotes::install_git("sounkou-bioinfo/RSimpleFFI")
```

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
platform-specific types. Types are C backed S7 objects, they hold in C
structs informations about alignments, sizes, libffi internal types that
are used when we call C functions.

### Basic Types

These are C basic types mostly mirroring libffi types

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
longlong_type <- ffi_longlong()
ulonglong_type <- ffi_ulonglong()

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
`ffi_alloc()` and `ffi_copy_array_type()`. They is a way to handle
fixed-size arrays and passed them to C functions via pointers

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
`ffi_alloc()`, `ffi_get_field()`, `ffi_set_field()` and allied

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

structs can be nested

``` r
# Define inner and outer struct types
inner_type <- ffi_struct(x = ffi_int32(), y = ffi_double())
outer_type <- ffi_struct(id = ffi_int32(), point = inner_type, label = ffi_string())

# Allocate memory for the outer_type struct
buf <- ffi_alloc_buffer(ffi_sizeof(outer_type))

# Set fields in the outer struct
ffi_set_field(buf, "id", 42L, outer_type)
ffi_set_field(buf, "label", "example", outer_type)

# Access the nested struct within the parent buffer
point_ptr <- ffi_get_field(buf, "point", outer_type)
ffi_set_field(point_ptr, "x", 10L, inner_type)
ffi_set_field(point_ptr, "y", 3.14, inner_type)

# Get fields from the outer struct
id <- ffi_get_field(buf, "id", outer_type)
label <- ffi_get_field(buf, "label", outer_type)

# Get fields from the nested struct
x <- ffi_get_field(point_ptr, "x", inner_type)
y <- ffi_get_field(point_ptr, "y", inner_type)
# Show results
id
#> [1] 42
x
#> [1] 10
y
#> [1] 3.14
label  # string fields are returned directly as character
#> [1] "example"
```

You can define more complex structs by adding more fields and using any
supported FFI type

### Struct Arrays

You can allocate contiguous arrays of structs and access elements by
index

``` r
# Define a Point struct
Point <- ffi_struct(x = ffi_int(), y = ffi_int())

# Allocate an array of 5 Points
points <- ffi_alloc(Point, 5L)

# Set values for each point
for (i in 1:5) {
  p <- ffi_get_element(points, i, Point)
  ffi_set_field(p, "x", as.integer(i * 10), Point)
  ffi_set_field(p, "y", as.integer(i * 20), Point)
}

# Read back values
p3 <- ffi_get_element(points, 3L, Point)
ffi_get_field(p3, "x", Point)
#> [1] 30
ffi_get_field(p3, "y", Point)
#> [1] 60
```

### Field Introspection

Use `ffi_field_info()`, `ffi_offsetof()`, `ffi_all_offsets()` and the
packed struct versions to inspect struct types’ layout

``` r
# Struct with alignment padding: int (4) + padding (4) + double (8) = 16 bytes
Mixed <- ffi_struct(a = ffi_int(), b = ffi_double())
ffi_sizeof(Mixed)
#> [1] 16

# Get byte offset of a field (like C's offsetof macro)
ffi_offsetof(Mixed, "a")
#> [1] 0
ffi_offsetof(Mixed, "b")  # offset 8 due to 8-byte alignment
#> [1] 8

# Get all offsets at once
ffi_all_offsets(Mixed)
#> a b 
#> 0 8

# Get detailed field info
ffi_field_info(Mixed, "b")
#> FieldInfo('b' type=double, offset=8, size=8)
```

### Struct Packing

By default, structs use natural alignment (platform ABI). Use the `pack`
parameter to control alignment, matching C’s `#pragma pack(n)`

``` r
# Natural alignment: int (4) + padding (4) + double (8) = 16 bytes
Natural <- ffi_struct(a = ffi_int(), b = ffi_double())
ffi_sizeof(Natural)
#> [1] 16

# Packed (no padding): int (4) + double (8) = 12 bytes
Packed <- ffi_struct(a = ffi_int(), b = ffi_double(), .pack = 1)
ffi_sizeof(Packed)
#> [1] 12

# Field offsets differ between packed and natural
ffi_offsetof(Natural, "b")  # 8 (aligned to 8-byte boundary)
#> [1] 8
ffi_offsetof(Packed, "b")   # 4 (immediately after int)
#> [1] 4
```

Pack values work like GCC/Clang/MSVC meaning each field’s alignment is
`min(natural_alignment, pack)`

``` r
# .pack=2: fields aligned to at most 2-byte boundaries
Pack2 <- ffi_struct(a = ffi_int(), b = ffi_double(), .pack = 2)
ffi_sizeof(Pack2)   # 12 bytes
#> [1] 12
ffi_offsetof(Pack2, "b")  # 4
#> [1] 4

# .pack=4: useful for matching 32-bit packed structures
Pack4 <- ffi_struct(a = ffi_int(), b = ffi_double(), .pack = 4)
ffi_sizeof(Pack4)  # 12 bytes
#> [1] 12
```

Use `ffi_all_offsets()` to see the complete layout

``` r
# Compare layouts
ffi_all_offsets(Natural)  # a=0, b=8
#> a b 
#> 0 8
ffi_all_offsets(Packed)   # a=0, b=4
#> a b 
#> 0 4
```

#### Packed Struct Limitation

Packed structs cannot be passed by value to C functions (libffi
limitation). Use pointers instead.

``` r
PackedPoint <- ffi_struct(x = ffi_uint8(), y = ffi_int32(), .pack = 1L)
tryCatch(
  ffi_cif(PackedPoint, ffi_int()),
  error = function(e) message(e$message)
)
#> Packed struct 'struct' cannot be passed by value to a C function (libffi limitation). Use a pointer instead.
```

### Enumerations

Enums map named constants to integer values, useful for flags, status
codes, or options:

``` r
# Define an enum type
Color <- ffi_enum(RED = 0L, GREEN = 1L, BLUE = 2L)

# Allocate enum value
color_ptr <- ffi_alloc(Color)

# Convert between names and integers
ffi_enum_to_int(Color, "GREEN")
#> [1] 1
ffi_int_to_enum(Color, 1L)
#> [1] "GREEN"

# Use enums in structs
Pixel <- ffi_struct(
  color = Color,
  intensity = ffi_uint8()
)

pixel <- ffi_alloc(Pixel)
ffi_set_field(pixel, "color", ffi_enum_to_int(Color, "RED"), Pixel)
ffi_set_field(pixel, "intensity", 255L, Pixel)

color_val <- ffi_get_field(pixel, "color", Pixel)
ffi_int_to_enum(Color, color_val)
#> [1] "RED"
```

### Unions

Unions allow multiple fields to share the same memory location, useful
for variant types or memory-efficient data structures:

``` r
# Define a union (all fields share the same memory)
Value <- ffi_union(
  as_int = ffi_int(),
  as_float = ffi_float(),
  as_bytes = ffi_array_type(ffi_uint8(), 4L)
)

# Allocate union
val <- ffi_alloc(Value)

# Write as integer
ffi_set_field(val, "as_int", 0x41424344L, Value)

# Read back as integer
ffi_get_field(val, "as_int", Value)
#> [1] 1094861636

# Read the same memory as float (reinterprets the bits)
ffi_get_field(val, "as_float", Value)
#> [1] 12.14142

# Tagged union example (union + tag field)
TaggedValue <- ffi_struct(
  tag = ffi_int(),  # 0=int, 1=float
  data = Value
)

tagged <- ffi_alloc(TaggedValue)
ffi_set_field(tagged, "tag", 1L, TaggedValue)

# Access nested union
data_ptr <- ffi_get_field(tagged, "data", TaggedValue)
ffi_set_field(data_ptr, "as_float", 3.14, Value)
ffi_get_field(data_ptr, "as_float", Value)
#> [1] 3.14
```

## Function Calling

After definining types for input and output arguments, we can called
availaible C callables after some call preparation. We need to find the
native function adress with functions like `ffi_symbol` (and the dll\_\*
versions) ,`ffi_cif` creates the context required for a libffi call. We
can then call using `ffi_call`. `ffi_function` is a more stremalined
version

### Basic Function Calls

The package comes with some built-in C test functions for testing, they
are defined in [src/test_functions.c](src/test_functions.c) and are
accessible when the package is loaded.

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

#### Some Integer Types examples

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

This calls a C function that returns a string (const char\*). With
`ffi_string()` type, strings are returned directly as R character
vectors, usually it is advised to use `ffi_pointer` and explicitly
convert them after. Care should be taken to the lifetime of the return
external pointers

``` r
string_func <- ffi_symbol("test_return_string")
string_cif <- ffi_cif(string_type)
string_result <- ffi_call(string_cif, string_func)
string_result  
#> [1] "Hello from C!"
```

#### Struct Types

For structs, arrays and more complex types, you need to allocate your
struct after defining the types. They allocated data are managed by R
finalizers so there should be no special concerns around their lifetime.

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

RSimpleFFI converts between R and C types following C99 semantics.
Doubles are truncated to integers, overflow uses modular arithmetic, and
negative values convert to unsigned types via two’s complement (e.g
`-1L` -\> `uint8` = 255). Large integers (64-bit) return as doubles and
may lose precision beyond 2^53.

``` r
# Integer truncation: 5.7 -> 5
int_type <- ffi_int()
add_fn <- ffi_function("test_add_int", int_type, int_type, int_type)
add_fn(5.7, 3.2)  # 5 + 3 = 8
#> [1] 8

# Signed overflow wraps (int8 range is -128 to 127)
int8_type <- ffi_int8()
int8_fn <- ffi_function("test_int8_func", int8_type, int8_type)
int8_fn(127L)  # 127 + 1 wraps to -128
#> [1] -128

# Unsigned modular arithmetic
uint8_type <- ffi_uint8()
uint8_fn <- ffi_function("test_uint8_func", uint8_type, uint8_type)
uint8_fn(-1L)  # -1 as uint8 = 255, then +1 wraps to 0
#> [1] 0

# Bool conversion
bool_type <- ffi_bool()
bool_fn <- ffi_function("test_bool_func", bool_type, bool_type)
bool_fn(TRUE)   # !TRUE = FALSE
#> [1] FALSE
bool_fn(42L)    # 42 != 0 -> TRUE, !TRUE = FALSE
#> [1] FALSE
```

#### NA Handling

By default, NA values in arguments raise an error to prevent silent data
corruption (`NA_integer_` becomes `INT_MIN`, `NA_real_` becomes a NaN
that C doesn’t recognize as missing).

``` r
# NA values cause errors by default
add_fn <- ffi_function("test_add_int", ffi_int(), ffi_int(), ffi_int())
add_fn(NA_integer_, 5L)  # Error: NA values in arguments
#> Error: NA value not allowed in argument 1. Use na_check=FALSE to allow (at your own risk).
```

If you know what you’re doing and want to pass NA values (e.g., when
working with sentinel values), you can disable the check:

``` r
# Disable NA check with na_check = FALSE
add_fn_unsafe <- ffi_function("test_add_int", ffi_int(), ffi_int(), ffi_int(), na_check = FALSE)
add_fn_unsafe(NA_integer_, 5L)  # Passes NA as INT_MIN
#> [1] -2147483643

# You can also use na_check in ffi_call and dll_ffi_symbol
cif <- ffi_cif(ffi_int(), ffi_int(), ffi_int())
sym <- ffi_symbol("test_add_int")
ffi_call(cif, sym, NA_integer_, 5L, na_check = FALSE)
#> [1] -2147483643
```

Note that `NaN` (Not a Number) values are allowed through because they
have well-defined IEEE 754 semantics that C understands:

``` r
# NaN passes through - it has IEEE semantics
double_fn <- ffi_function("test_add_double", ffi_double(), ffi_double(), ffi_double())
double_fn(NaN, 5.0)  # NaN + 5 = NaN
#> [1] NaN
```

### Closures and R callbacks for C

The closure API lets you wrap R functions as C callbacks. This is useful
when C code expects a function pointer (e.g., qsort comparator, event
handlers) and you want to use an R function

``` r
# Create an R function to use as callback
add_ten <- function(x) {
 print(paste0("adding " , x, " to 10"))
 as.integer(x + 10L)
}

# Wrap it: int (*callback)(int)
closure <- ffi_closure(add_ten, ffi_int(), ffi_int())

# Get the function pointer to pass to C
callback_ptr <- ffi_closure_pointer(closure)

# Call C function that accepts a callback: test_callback(func, value)
test_callback_fn <- ffi_function(
 "test_callback",
 ffi_int(),
 ffi_pointer(),
 ffi_int()
)

test_callback_fn(callback_ptr, 5L) 
#> [1] "adding 5 to 10"
#> [1] 15
```

Closures work with any signature RSimpleFFI supports

``` r
# double (*transform)(double)
square <- function(x) x * x
square_closure <- ffi_closure(square, ffi_double(), ffi_double())

test_double_callback_fn <- ffi_function(
 "test_double_callback",
 ffi_double(),
 ffi_pointer(),
 ffi_double()
)

test_double_callback_fn(ffi_closure_pointer(square_closure), 4.0)
#> [1] 16
```

### Call system libraries functions or external shared libraries

You can load external shared libraries at runtime using RSimpleFFI’s
`dll_load()` and `dll_ffi_symbol()` functions. These are wrappers around
R’s native `dyn.load()` facilities that search for shared libraries in
system paths when required. It is advised to used the other more
explicit path definition version

#### Search so files in system paths

On some systems libc symbols are not already accessible in the R
process’ adress space, so this example loads it

``` r
# Example: call the C standard library rand() function
libc_path <- dll_load_system("libc.so.6")
#> Loading system library from: /usr/lib/x86_64-linux-gnu/libc.so.6
rand_func <- dll_ffi_symbol("rand", ffi_int())
rand_value <- rand_func()
rand_value
#> [1] 402957072
rand_value <- rand_func()
rand_value
#> [1] 704983047
dll_unload(libc_path)
```

#### Explicitly load shared libraries

This is the preferred method to load libraries

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
#> <pointer: 0x60aa2c726c60>

# Read back the buffer and print as string
rawToChar(ffi_copy_array(buf_ptr, 8L, raw_type))
#> [1] "AAAAAAAA"

pointer_to_string(buf_ptr)
#> [1] "AAAAAAAA"

dll_unload(lib_path)
```

### Compile and Load C Code

The package provides facilities to load C code on the fly. The
compilation uses `R CMD SHLIB` under the hood

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
overhead of calling C functions from R using RSimpleFFI ) compared to
native R C built-in functions. We would expect some overhead of the
marshalling performed by the package and libffi. So this will be added
to the `.Call` overhead. So for more performance critical applications
and availability of time, making the usual `.Call` interface `SEXP`
wrappers is more advisable

### R builtin C functions

Here we compare to the builtin R C functions

``` r

set.seed(1995)
n <- 100000
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
#> 1 native_r      266µs    292µs     3403.     781KB     34.4
#> 2 ffi_call      392µs    399µs     2459.     782KB     50.2
dll_unload(lib_path)
```

### Compared to interpreted R Code

Let’s compare a pure C convolution to a simple R implementation. This is
classic benchmark due to the loop overhead in R even with the JIT

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
signal <- rnorm(100000)
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
#> <pointer: 0x60aa31af4b20>
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
#> 1 r            24.1ms   24.3ms      41.0     781KB     27.3
#> 2 c_ffi       397.8µs  455.3µs    2088.      782KB    110.

dll_unload(lib_path)
```

## Some `Dangerous` Call to R API Exported Symbols

Since `libR.so` is loaded (on unix systems) we can use its exported
symbols and functions. This allows calling R’s internal C API directly
via FFI - the same functions that R packages use in their C code. This
is for educational purposes only! Calling R internals incorrectly can
crash R or corrupt memory.

``` r
# Rf_ScalarInteger - create an R integer scalar (returns SEXP pointer)
rf_ScalarInteger <- ffi_function("Rf_ScalarInteger", ffi_pointer(), ffi_int())
rf_ScalarReal <- ffi_function("Rf_ScalarReal", ffi_pointer(), ffi_double())

# Create R objects via C API
int_sexp <- rf_ScalarInteger(42L)
dbl_sexp <- rf_ScalarReal(3.14159)

# Extract values using INTEGER_ELT / REAL_ELT
rf_INTEGER_ELT <- ffi_function("INTEGER_ELT", ffi_int(), ffi_pointer(), ffi_long())
rf_REAL_ELT <- ffi_function("REAL_ELT", ffi_double(), ffi_pointer(), ffi_long())

rf_INTEGER_ELT(int_sexp, 0L)
#> [1] 42
rf_REAL_ELT(dbl_sexp, 0L)
#> [1] 3.14159
```

We can also create and extract strings:

``` r
# Rf_mkString creates a STRSXP (character vector)
rf_mkString <- ffi_function("Rf_mkString", ffi_pointer(), ffi_string())
rf_STRING_ELT <- ffi_function("STRING_ELT", ffi_pointer(), ffi_pointer(), ffi_long())
rf_R_CHAR <- ffi_function("R_CHAR", ffi_string(), ffi_pointer())

str_sexp <- rf_mkString("Hello from C!")
char_sexp <- rf_STRING_ELT(str_sexp, 0L)
rf_R_CHAR(char_sexp)
#> [1] "Hello from C!"
```

We can also read global variables from shared libraries using
`ffi_deref_pointer()` and `ffi_read_global()`:

``` r
# Read global variables from our test library
int_addr <- getNativeSymbolInfo("test_global_int", "RSimpleFFI")$address
ffi_read_global(int_addr, ffi_int())
#> [1] 42

dbl_addr <- getNativeSymbolInfo("test_global_double", "RSimpleFFI")$address
ffi_read_global(dbl_addr, ffi_double())
#> [1] 3.14159

# For pointer globals, use ffi_deref_pointer
str_ptr_addr <- getNativeSymbolInfo("test_global_string", "RSimpleFFI")$address
pointer_to_string(ffi_deref_pointer(str_ptr_addr))
#> [1] "Hello from global!"
```

Now let’s call R functions entirely through the C API:

``` r
# Get R_GlobalEnv - it's a global pointer variable
R_GlobalEnv <- ffi_deref_pointer(getNativeSymbolInfo("R_GlobalEnv")$address)

# Define R API functions
rf_install <- ffi_function("Rf_install", ffi_pointer(), ffi_string())
rf_lang1 <- ffi_function("Rf_lang1", ffi_pointer(), ffi_pointer())
rf_lang2 <- ffi_function("Rf_lang2", ffi_pointer(), ffi_pointer(), ffi_pointer())
rf_eval <- ffi_function("Rf_eval", ffi_pointer(), ffi_pointer(), ffi_pointer())

# Call Sys.time() entirely via R's C API!
sys_time_sym <- rf_install("Sys.time")
call_expr <- rf_lang1(sys_time_sym)
result <- rf_eval(call_expr, R_GlobalEnv)
rf_REAL_ELT(result, 0L)  # Unix timestamp
#> [1] 1765045606

# Call abs(-42) via C API
abs_sym <- rf_install("abs")
neg_val <- rf_ScalarInteger(-42L)
abs_call <- rf_lang2(abs_sym, neg_val)
abs_result <- rf_eval(abs_call, R_GlobalEnv)
rf_INTEGER_ELT(abs_result, 0L)
#> [1] 42
```

## Header Parsing and Code Generation

RSimpleFFI can parse C header files using
[tinycc](https://github.com/tinycc/tinycc) to generate preprocessed
headers, use the c grammar and treesitter.c package to automatically
generate R bindings, making it easy to create R packages that wrap C
libraries. These will required manual review. It is preferable to not
include system includes to avoid junk type definitons and helpers which
are of no interest or clash with tinycc’s standard library

### Parse Headers

``` r
# Parse a C header file using tinycc preprocessor
header_file <- system.file("extdata", "simple_types.h", package = "RSimpleFFI")
parsed <- ffi_parse_header(header_file)

# Inspect what was found
names(parsed$defines)
#> [1] "SIMPLE_TYPES_H" "MAX_BUFFER"     "MIN_SIZE"       "SIMPLE_TYPES_H"
#> [5] "MAX_BUFFER"     "MIN_SIZE"
names(parsed$structs)
#> [1] "Point"
parsed$functions$name
#> [1] "add"           "multiply"      "process_point"
```

### Generate R Bindings

``` r
# Generate complete R code
code <- generate_r_bindings(parsed)

# Preview first part of generated code
substr(code, 1, 500)
#> [1] "# Auto-generated R bindings for simple_types.h\n# Generated on: 2025-12-06 19:26:46.344175\n# Source hash: d3eba819d380b57852bd0b9edb3e1f5a\n#\n# NOTE: These functions expect symbols to be available in the current process.\n# For external libraries, load them first with dll_load() or use dll_ffi_symbol().\n#\n# Type handling:\n#  - Primitives (int, double, etc.): passed by value, auto-converted\n#  - char*: use ffi_pointer(), use pointer_to_string() for conversion to string\n#  - struct Foo*: use ffi_poin"

# The generated code includes:
# - Constants from #define
# - ffi_struct() definitions for structs
# - Wrapper functions with roxygen2 documentation
```

### Source Generated Code

``` r
# Write to temp file and source it
tmpfile <- tempfile(fileext = ".R")
writeLines(code, tmpfile)

# Source the generated bindings
source(tmpfile)

# Now we can use the generated constants and structs
MAX_BUFFER
#> [1] 1024
MIN_SIZE
#> [1] 16

# The Point struct is now available
Point
#> StructType(fields=[x, y], size=8)
#> Fields:
#>   x: FFIType(int, size=4)
#>   y: FFIType(int, size=4)

# Clean up
unlink(tmpfile)
```

### Generate Bindings for System Libraries

Let’s create bindings for real C library functions. This is the prefered
way to avoid extraneous includes.

``` r
# Create a simple header with libc function signatures
libc_header <- tempfile(fileext = ".h")
writeLines(c(
  "// String functions", 
  "unsigned long strlen(const char* s);",
  "int strcmp(const char* s1, const char* s2);",
  "",
  "// Math functions",
  "int abs(int n);"
), libc_header)

# Parse and generate bindings
libc_parsed <- ffi_parse_header(libc_header)
libc_code <- generate_r_bindings(libc_parsed)

# Preview generated code
cat(substr(libc_code, 1, 600))
#> # Auto-generated R bindings for file108fec50ae86c3.h
#> # Generated on: 2025-12-06 19:26:46.377157
#> # Source hash: 2b4c2eff17ca02fc5e637d979740174c
#> #
#> # NOTE: These functions expect symbols to be available in the current process.
#> # For external libraries, load them first with dll_load() or use dll_ffi_symbol().
#> #
#> # Type handling:
#> #  - Primitives (int, double, etc.): passed by value, auto-converted
#> #  - char*: use ffi_pointer(), use pointer_to_string() for conversion to string
#> #  - struct Foo*: use ffi_pointer(), allocate with ffi_struct() + ffi_alloc()
#> #  - Struct fields: access with ffi_get_field(

# Source the bindings
tmpfile <- tempfile(fileext = ".R")
writeLines(libc_code, tmpfile)
source(tmpfile)

# Load libc
libc_path <- dll_load_system("libc.so.6")
#> Loading system library from: /usr/lib/x86_64-linux-gnu/libc.so.6

# Use generated wrapper functions!
r_strlen("Hello, World!")
#> [1] 13

r_strcmp("apple", "banana")
#> [1] -1

r_strcmp("test", "test")
#> [1] 0

r_abs(-42L)
#> [1] 42

dll_unload(libc_path)
unlink(c(tmpfile, libc_header))
```

### Example: Streaming Genomics Data with htslib

Generate bindings for [htslib](https://github.com/samtools/htslib) with
libcurl support and stream VCF data from the 1000 Genomes Project. We’ve
installed htslib version 1.22 and generate the bindings

``` r
# Generate htslibFFI package from custom htslib installation with libcurl support
htslib_root <- "../htslib-install"
if(!requireNamespace("htslibFFI"))
system(sprintf("Rscript tools/generate_htslib_package.R /tmp/htslibFFI %s", htslib_root),
           ignore.stdout = TRUE, ignore.stderr = TRUE)
#> Loading required namespace: htslibFFI
#> Loading system library from: /usr/lib/x86_64-linux-gnu/libhts.so.3

# Clean up name conflicts from earlier examples
rm(list = c("double_t", "r_abs", "r_strcmp", "r_strlen"), envir = .GlobalEnv)

library(htslibFFI)
# Open VCF file directly from URL
vcf_url <- paste0(
  "https://ftp.1000genomes.ebi.ac.uk/vol1/ftp/data_collections/",
  "1000G_2504_high_coverage/working/20220422_3202_phased_SNV_INDEL_SV/",
  "1kGP_high_coverage_Illumina.chr21.filtered.SNV_INDEL_SV_phased_panel.vcf.gz"
)
fp <- r_hts_open(vcf_url, "r")
hdr <- r_bcf_hdr_read(fp)
# Read first 3 variants
rec <- r_bcf_init()
tmpfile <- tempfile(fileext = ".vcf")
outfp <- r_hts_open(tmpfile, "w")
r_vcf_hdr_write(outfp, hdr)
#> [1] 0
count <- 0
while (count < 3) {
  if (r_bcf_read(fp, hdr, rec) < 0) break
  r_bcf_unpack(rec, 15L)
  r_vcf_write(outfp, hdr, rec)
  count <- count + 1
}
# cleanup in C
r_hts_close(outfp)
#> [1] 0
r_bcf_destroy(rec)
#> NULL
r_bcf_hdr_destroy(hdr)
#> NULL
r_hts_close(fp)
#> [1] 0
# Display variants
vcf_lines <- readLines(tmpfile)
data_lines <- vcf_lines[-(1:grep("^#CHROM", vcf_lines))]
variants <- character()
for (line in data_lines) {
  if (nchar(line) > 0) {
    fields <- strsplit(line, "\t")[[1]]
    variants <- c(variants, sprintf("chr%s:%s %s>%s", fields[1], fields[2], fields[4], fields[5]))
  }
}
variants
#> [1] "chrchr21:5030578 C>T" "chrchr21:5030588 T>C" "chrchr21:5030596 A>G"
unlink(tmpfile)
```

The generator automatically creates 400+ function wrappers with proper
`.onLoad()` hooks and library path handling.

### Parsing R’s C API

The `bindgen_r_api()` function parses R’s own header files
(Rinternals.h, Rmath.h) and generates FFI bindings. This provides access
to R’s internal C functions without writing C code. Educational purpose
only and to test the parsing code logic ! Scripts in [tools](./tools)
will provide more interesting examples.

``` r
result <- bindgen_r_api(headers = c("Rinternals.h", "Rmath.h"))
names(result)
#> [1] "Rinternals" "Rmath"
length(result$Rinternals$functions)
#> [1] 5
names(result$Rinternals$enums)
#> [1] "Rboolean"           "NativeSymbolType"   "nchar_type"        
#> [4] "cetype_t"           "R_pstream_format_t"
```

Generate bindings and call statistical distribution functions directly

``` r
outfile <- tempfile(fileext = ".R")
bindgen_r_api(output_file = outfile, headers = "Rmath.h")
#> Generated R bindings written to: /tmp/RtmpGphZFr/file108fec79a2f1b8.R
source(outfile)

r_Rf_dnorm4(0, 0, 1, 0L)
#> [1] 0.3989423
dnorm(0, 0, 1)
#> [1] 0.3989423

r_Rf_pnorm5(1.96, 0, 1, 1L, 0L)
#> [1] 0.9750021
pnorm(1.96)
#> [1] 0.9750021

r_Rf_qnorm5(0.975, 0, 1, 1L, 0L)
#> [1] 1.959964
qnorm(0.975)
#> [1] 1.959964

r_Rf_gammafn(5)
#> [1] 24
gamma(5)
#> [1] 24

unlink(outfile)
```

### Create R Packages

We can create R package scafolds from some headers

``` r
# Generate complete package scaffolding
tmpdir <- tempfile()
dir.create(tmpdir)

generate_package_from_headers(
  header_files = header_file,
  package_name = "MyRPackage",
  library_name = "mylib",
  output_dir = tmpdir,
  use_system_lib = TRUE,
  authors_r = 'person("John", "Doe", email = "john@example.com", role = c("aut", "cre"))'
)

# Check what was created - proper R package structure
list.files(tmpdir)
#> [1] "DESCRIPTION" "LICENSE"     "NAMESPACE"   "R"
list.files(file.path(tmpdir, "R"))
#> [1] "helpers.R"               "simple_types_bindings.R"
#> [3] "zzz.R"
```

### Binding Generation

The `bindgen_r_api()` function parses R’s C headers and generates
wrapper functions automatically. This is for testing the parser mostly,
using this is discourage !

``` r
# Generate bindings for all R headers
bindgen_r_api(output_file = "r_bindings.R", verbose = TRUE)
```

### Helper Functions

``` r
# Convert struct to/from R lists
Point <- ffi_struct(x = ffi_int(), y = ffi_int())

pt <- ffi_struct_from_list(Point, list(x = 10L, y = 20L))
values <- ffi_struct_to_list(pt, Point)
values
#> $x
#> [1] 10
#> 
#> $y
#> [1] 20

# Create arrays of structs
points <- ffi_struct_array_from_list(Point, list(
  list(x = 0L, y = 0L),
  list(x = 10L, y = 20L)
))

# Pretty print structs
ffi_print_struct(pt, Point)
#> Struct (struct):
#>   x (int): 10 
#>   y (int): 20
```

### SEXP Pointer Helpers

When calling R’s internal C API via FFI, you need pointers to R objects
(SEXP). The `sexp_ptr()` and `data_ptr()` helpers extract pointers while
preventing garbage collection via `R_PreserveObject`. Protection is
automatically released when the pointer is garbage collected.

``` r
x <- c(1L, 2L, 3L, 4L, 5L)
ptr <- sexp_ptr(x)
ptr
#> <pointer: 0x60aa2fb8f928>

# Call Rf_length via FFI
rf_length <- ffi_function("Rf_length", ffi_int(), ffi_pointer())
rf_length(ptr)
#> [1] 5
```

## Limitations and issues of note

Current limitations include some unnecessary copying, potential memory
leaks and the fact that our type objects are C pointers that are never
and should never be finalized.
[`rchck`](https://github.com/kalibera/rchk) is regularly used to detect
potential issues. Care should be taken to the lifetime of returned and
create memory because we are interacting like we are in `C` after all !
Some other specific issues are described below

### Bit-fields

Bit-field structs are laid out by the compiler as compact integers.
While they are structs in C, `ffi_struct()` models each field as a
separate full-sized type, mismatching the actual memory layout.

Example:

``` c
typedef struct { unsigned enabled:1; mode:3; priority:4; reserved:24; } SettingsFlags;
int get_mode(SettingsFlags settings);
SettingsFlags increment(SettingsFlags settings);
```

Modeling with `ffi_struct(enabled=ffi_int(), mode=ffi_int(), ...)`
creates a 16-byte struct (4 fields × 4 bytes), but the C compiler packs
`SettingsFlags` into 4 bytes (32 bits total).

Model bit-field structs as their packed integer type. Sum bit widths
(1+3+4+24 = 32 bits) and use the corresponding `ffi_uintN()` type
(`ffi_uint32()` here).

``` r
# C functions that take/return SettingsFlags (32-bit bit-field struct)
# Model the struct as uint32_t in the FFI signature

# Create a packed value (enabled=1, mode=5, priority=10)
pack_fn <- ffi_function("test_bitfield_pack", ffi_uint32(), ffi_int(), ffi_int(), ffi_int())
packed <- pack_fn(1L, 5L, 10L)
packed
#> [1] 171

# Pass bit-field struct by value (as uint32_t)
get_mode_fn <- ffi_function("test_bitfield_struct_get_mode", ffi_int(), ffi_uint32())
get_mode_fn(packed)
#> [1] 5

# Return bit-field struct by value (as uint32_t)
increment_fn <- ffi_function("test_bitfield_struct_increment_priority", ffi_uint32(), ffi_uint32())
incremented <- increment_fn(packed)

# Verify the change
get_priority_fn <- ffi_function("test_bitfield_get_priority", ffi_int(), ffi_uint32())
get_priority_fn(packed)      # Original: 10
#> [1] 10
get_priority_fn(incremented) # After increment: 11
#> [1] 11
```

Bit-field structs passed by pointer:

``` r
# C functions that take SettingsFlags* (pointer to bit-field struct)

# Allocate buffer for 32-bit bit-field struct
buf <- ffi_alloc(ffi_int(), 1L)  # uint32_t and int are same size
pack_fn <- ffi_function("test_bitfield_pack", ffi_uint32(), ffi_int(), ffi_int(), ffi_int())
packed <- pack_fn(1L, 3L, 7L)
ffi_fill_typed_buffer(buf, as.integer(packed), ffi_int())
#> NULL

# Read field via pointer to struct
ptr_get_fn <- ffi_function("test_bitfield_struct_ptr_get_mode", ffi_int(), ffi_pointer())
ptr_get_fn(buf)
#> [1] 3

# Modify field via pointer to struct
ptr_set_fn <- ffi_function("test_bitfield_struct_ptr_set_mode", ffi_void(), ffi_pointer(), ffi_int())
ptr_set_fn(buf, 5L)
#> NULL
ptr_get_fn(buf)  # Verify: now 5
#> [1] 5
```

For convenient access, use bitfield helpers:

``` r
settings <- ffi_create_bitfield_accessors(
  list(enabled = 1L, mode = 3L, priority = 4L)
)

packed <- settings$pack(list(enabled = 1L, mode = 5L, priority = 10L))
settings$get(packed, "mode")
#> [1] 5
settings$get(packed, "priority")
#> [1] 10
```

Some helper functions `ffi_pack_bits()`, `ffi_unpack_bits()`,
`ffi_extract_bit_field()`, `ffi_set_bit_field()`,
`ffi_create_bitfield_accessors()`.

### 64-bit Bitfields

For bitfields exceeding 32 bits or requiring full 64-bit range, use the
64-bit variants:

``` r
# Pack fields totaling more than 32 bits
packed64 <- ffi_pack_bits64(c(1L, 0x7FFFFFFFL), c(1L, 31L))
packed64
#> [1] 4294967295

# Extract fields
ffi_unpack_bits64(packed64, c(1L, 31L))
#> [1]          1 2147483647

# Single field extraction
ffi_extract_bits64(packed64, 0L, 1L)   # First field
#> [1] 1
ffi_extract_bits64(packed64, 1L, 31L)  # Second field
#> [1] 2147483647
```

### Signed Bitfields

For signed integer fields (common in hardware registers), use signed
extraction:

``` r
# Pack value 13 (0xD) which represents -3 in signed 4-bit
packed <- ffi_pack_bits(c(13L), c(4L))

# Unsigned extraction returns 13
ffi_extract_bit_field(packed, 0L, 4L)
#> [1] 13

# Signed extraction returns -3 (sign-extended)
ffi_extract_signed_bit_field(packed, 0L, 4L)
#> [1] -3

# 64-bit signed extraction also available
ffi_extract_signed_bits64(as.double(packed), 0L, 4L)
#> [1] -3
```

### Structs & Unions By Value

Struct/union passing by value may be unreliable across platforms. Packed
structs cannot be passed by value (libffi limitation). Prefer pointers.

# License

This project is licensed under the GPL-3 License.

# References

- [Rffi](https://github.com/omegahat/Rffi)

- [libffi](https://github.com/libffi/libffi) - Portable foreign function
  interface library

- [tinycc](https://github.com/tinycc/tinycc) - Tiny C Compiler used for
  header parsing

- [libffi
  Examples](http://www.chiark.greenend.org.uk/doc/libffi-dev/html/Using-libffi.html)

- [CPython’s ctypes
  module](https://docs.python.org/3/library/ctypes.html)
