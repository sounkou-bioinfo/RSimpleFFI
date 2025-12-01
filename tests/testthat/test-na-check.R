# Test NA checking functionality

library(testthat)
library(RSimpleFFI)

test_that("NA values in integer arguments cause error by default", {
  int_t <- ffi_int()
  add_fn <- ffi_function("test_add_int", int_t, int_t, int_t)
  
  # Normal call works
  expect_equal(add_fn(1L, 2L), 3L)
  
 # NA in first argument
  expect_error(add_fn(NA_integer_, 2L), "NA value not allowed")
  
  # NA in second argument  
  expect_error(add_fn(1L, NA_integer_), "NA value not allowed")
})

test_that("NA values in double arguments cause error by default", {
  double_t <- ffi_double()
  add_fn <- ffi_function("test_add_double", double_t, double_t, double_t)
  
  # Normal call works
  expect_equal(add_fn(1.5, 2.5), 4.0)
  
  # NA causes error
  expect_error(add_fn(NA_real_, 2.5), "NA value not allowed")
  expect_error(add_fn(1.5, NA_real_), "NA value not allowed")
})

test_that("NaN is allowed through (IEEE 754 semantics)", {
  double_t <- ffi_double()
  add_fn <- ffi_function("test_add_double", double_t, double_t, double_t)
  
  # NaN should pass through and propagate
  result <- add_fn(NaN, 2.5)
  expect_true(is.nan(result))
  
  result <- add_fn(1.5, NaN)
  expect_true(is.nan(result))
})

test_that("Inf is allowed through", {
  double_t <- ffi_double()
  add_fn <- ffi_function("test_add_double", double_t, double_t, double_t)
  
  # Inf should pass through
  expect_equal(add_fn(Inf, 1.0), Inf)
  expect_equal(add_fn(-Inf, 1.0), -Inf)
})

test_that("NA values in logical arguments cause error", {
  bool_t <- ffi_bool()
  bool_fn <- ffi_function("test_bool_func", bool_t, bool_t)
  
  # Normal call works
  expect_equal(bool_fn(TRUE), FALSE)
  expect_equal(bool_fn(FALSE), TRUE)
  
  # NA causes error
  expect_error(bool_fn(NA), "NA value not allowed")
})

test_that("na_check = FALSE allows NA through", {
  int_t <- ffi_int()
  add_fn <- ffi_function("test_add_int", int_t, int_t, int_t, na_check = FALSE)
  
  # With na_check = FALSE, NA_integer_ passes through as INT_MIN
  # test_add_int returns a + b, so INT_MIN + 1 = -2147483647
  result <- add_fn(NA_integer_, 1L)
  expect_equal(result, -2147483647L)
})

test_that("na_check can be set per-call", {
  int_t <- ffi_int()
  cif <- ffi_cif(int_t, int_t, int_t)
  sym <- ffi_symbol("test_add_int")
  
  # Default na_check = TRUE causes error
  expect_error(ffi_call(cif, sym, NA_integer_, 1L), "NA value not allowed")
  
  # Explicit na_check = FALSE allows through
  result <- ffi_call(cif, sym, NA_integer_, 1L, na_check = FALSE)
  expect_equal(result, -2147483647L)
})

test_that("dll_ffi_symbol respects na_check parameter", {
  # Load libc for a simple function
  skip_on_os("windows")
  
  libc_path <- dll_load_system("libc.so.6")
  on.exit(dll_unload(libc_path), add = TRUE)
  
  # abs() function: int abs(int)
  int_t <- ffi_int()
  
  # With na_check = TRUE (default)
  abs_fn <- dll_ffi_symbol("abs", int_t, int_t)
  expect_error(abs_fn(NA_integer_), "NA value not allowed")
  
  # With na_check = FALSE  
  abs_fn_no_check <- dll_ffi_symbol("abs", int_t, int_t, na_check = FALSE)
  # abs(INT_MIN) is undefined behavior in C, but won't error in R
  result <- abs_fn_no_check(NA_integer_)
  expect_type(result, "integer")
})

test_that("NA check works with string type", {
  string_t <- ffi_string()
  int_t <- ffi_int()
  
  strlen_fn <- ffi_function("test_string_length", int_t, string_t)
  
  # Normal call works
  expect_equal(strlen_fn("hello"), 5L)
  
  # NA string causes error
  expect_error(strlen_fn(NA_character_), "NA value not allowed")
})

test_that("External pointers bypass NA check", {
  # External pointers cannot contain NA, so should pass through
  int_t <- ffi_int()
  pointer_t <- ffi_pointer()
  
  point_type <- ffi_struct(x = ffi_int(), y = ffi_double())
  point_ptr <- ffi_alloc(point_type)
  ffi_set_field(point_ptr, "x", 42L, point_type)
  
  # This should work - external pointers don't have NA
  get_x_fn <- ffi_function("test_get_point_x", int_t, pointer_t)
  expect_equal(get_x_fn(point_ptr), 42L)
})

test_that("Error message mentions na_check=FALSE", {
  int_t <- ffi_int()
  add_fn <- ffi_function("test_add_int", int_t, int_t, int_t)
  
  # Check that error message is helpful
  expect_error(
    add_fn(NA_integer_, 1L), 
    "na_check.*FALSE"
  )
})
