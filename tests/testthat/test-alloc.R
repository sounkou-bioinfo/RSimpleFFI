# Ensure package functions are available
library(RSimpleFFI)
# Tests for ffi_alloc (typed buffer allocation)

test_that("ffi_alloc allocates int buffer and returns pointer", {
  int_type <- ffi_int()
  ptr <- ffi_alloc(int_type, 5L)
  expect_true(inherits(ptr, "externalptr"))
  expect_false(is_null_pointer(ptr))
})

test_that("ffi_alloc allocates double buffer and returns pointer", {
  double_type <- ffi_double()
  ptr <- ffi_alloc(double_type, 3L)
  expect_true(inherits(ptr, "externalptr"))
  expect_false(is_null_pointer(ptr))
})

test_that("ffi_alloc returns distinct pointers for each call", {
  int_type <- ffi_int()
  ptr1 <- ffi_alloc(int_type, 2L)
  ptr2 <- ffi_alloc(int_type, 2L)
  expect_false(identical(ptr1, ptr2))
})

test_that("ffi_alloc errors for invalid n", {
  int_type <- ffi_int()
  expect_error(ffi_alloc(int_type, 0L))
  expect_error(ffi_alloc(int_type, -1L))
})

test_that("ffi_alloc errors for invalid type", {
  expect_error(ffi_alloc("not_a_type", 2L))
})
