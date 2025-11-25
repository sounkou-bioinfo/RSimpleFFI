# ArrayType and Array FFI Tests for RSimpleFFI

library(testthat)
library(RSimpleFFI)

context("ArrayType and array FFI")

test_that("ArrayType can be created and has correct properties", {
  int_t <- ffi_int()
  arr_t <- ffi_array_type(int_t, 5L)
  expect_s7_class(arr_t, ArrayType)
  expect_equal(arr_t@element_type@name, "int")
  expect_equal(arr_t@length, 5L)
  expect_gt(arr_t@size, 0)
})

test_that("ffi_alloc works for ArrayType", {
  int_t <- ffi_int()
  arr_t <- ffi_array_type(int_t, 4L)
  ptr <- ffi_alloc(arr_t)
  expect_true(inherits(ptr, "externalptr"))
  expect_false(is_null_pointer(ptr))
})

test_that("ffi_copy_array_type copies C array to R vector", {
  int_t <- ffi_int()
  arr_t <- ffi_array_type(int_t, 3L)
  ptr <- ffi_alloc(arr_t)
  vals <- as.integer(c(10L, 20L, 30L))
  ffi_fill_typed_buffer(ptr, vals, int_t)
  out <- ffi_copy_array_type(ptr, arr_t)
  expect_equal(out, vals)
})

# Add more tests for nested structs with arrays as needed
