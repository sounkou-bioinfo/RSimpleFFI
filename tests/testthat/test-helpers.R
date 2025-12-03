# Tests for Helper Functions

library(testthat)
library(RSimpleFFI)

test_that("ffi_struct_from_list creates struct from list", {
  Point <- ffi_struct(x = ffi_int(), y = ffi_int())
  
  pt <- ffi_struct_from_list(Point, list(x = 10L, y = 20L))
  
  expect_type(pt, "externalptr")
  expect_equal(ffi_get_field(pt, "x", Point), 10L)
  expect_equal(ffi_get_field(pt, "y", Point), 20L)
})

test_that("ffi_struct_to_list converts struct to list", {
  Point <- ffi_struct(x = ffi_int(), y = ffi_int())
  
  pt <- ffi_alloc(Point)
  ffi_set_field(pt, "x", 42L, Point)
  ffi_set_field(pt, "y", 100L, Point)
  
  result <- ffi_struct_to_list(pt, Point)
  
  expect_type(result, "list")
  expect_equal(names(result), c("x", "y"))
  expect_equal(result$x, 42L)
  expect_equal(result$y, 100L)
})

test_that("ffi_struct_array_from_list creates array of structs", {
  Point <- ffi_struct(x = ffi_int(), y = ffi_int())
  
  values <- list(
    list(x = 0L, y = 0L),
    list(x = 10L, y = 20L),
    list(x = 30L, y = 40L)
  )
  
  arr <- ffi_struct_array_from_list(Point, values)
  
  expect_type(arr, "externalptr")
  
  # Verify values (would need pointer arithmetic to fully test)
  first <- ffi_struct_to_list(arr, Point)
  expect_equal(first$x, 0L)
  expect_equal(first$y, 0L)
})

test_that("ffi_print_struct displays struct contents", {
  Point <- ffi_struct(x = ffi_int(), y = ffi_int())
  
  pt <- ffi_struct_from_list(Point, list(x = 42L, y = 100L))
  
  # Capture output
  output <- capture.output(ffi_print_struct(pt, Point))
  
  expect_true(any(grepl("Struct", output)))
  expect_true(any(grepl("x.*42", output)))
  expect_true(any(grepl("y.*100", output)))
})

test_that("ffi_is_null detects NULL pointers", {
  Point <- ffi_struct(x = ffi_int(), y = ffi_int())
  pt <- ffi_alloc(Point)
  expect_false(ffi_is_null(pt))
})
