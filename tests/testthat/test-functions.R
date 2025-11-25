# Basic Function Call Tests

library(testthat)
library(RSimpleFFI)

# Helper to load test functions
load_test_lib <- function() {
  if (!is.loaded("test_add_double")) {
    # Try to load from current package or built library
    tryCatch(
      {
        dyn.load("RSimpleFFI.so")
      },
      error = function(e) {
        # Skip tests if library not available
        skip("Test library not available")
      }
    )
  }
}

test_that("Basic math function calls work", {
  load_test_lib()

  double_t <- ffi_double()
  int_t <- ffi_int()

  # Test double addition
  add_fn <- ffi_function("test_add_double", double_t, double_t, double_t)
  result <- add_fn(3.5, 2.5)
  expect_equal(result, 6.0)

  # Test integer addition
  add_int_fn <- ffi_function("test_add_int", int_t, int_t, int_t)
  result <- add_int_fn(5L, 3L)
  expect_equal(result, 8L)

  # Test square function
  square_fn <- ffi_function("test_square", double_t, double_t)
  result <- square_fn(4.0)
  expect_equal(result, 16.0)
})

test_that("Float operations work", {
  load_test_lib()

  float_t <- ffi_float()

  add_float_fn <- ffi_function("test_add_float", float_t, float_t, float_t)
  result <- add_float_fn(1.5, 2.5)
  expect_equal(result, 4.0, tolerance = 1e-6)
})

test_that("Void functions work", {
  load_test_lib()

  void_t <- ffi_void()

  void_fn <- ffi_function("test_void_function", void_t)
  result <- void_fn()
  expect_null(result)
})

test_that("Standard library functions work", {
  # Test our own factorial function instead of external libraries
  int_t <- ffi_int()

  factorial_fn <- ffi_function("test_factorial", int_t, int_t)
  result <- factorial_fn(5L)
  expect_equal(result, 120L) # 5! = 120

  result2 <- factorial_fn(4L)
  expect_equal(result2, 24L) # 4! = 24

  result3 <- factorial_fn(3L)
  expect_equal(result3, 6L) # 3! = 6
})

test_that("CIF creation and manual calls work", {
  double_t <- ffi_double()

  # Create CIF manually
  cif <- ffi_cif(double_t, double_t)
  expect_s7_class(cif, RSimpleFFI::CIF)

  # Get symbol manually
  symbol <- ffi_symbol("test_square")
  expect_s7_class(symbol, RSimpleFFI::NativeSymbol)
  expect_equal(symbol@name, "test_square")

  # Make call manually
  result <- ffi_call(cif, symbol, 3.0)
  expect_equal(result, 9.0)

  # Test with character symbol name
  result2 <- ffi_call(cif, "test_square", 5.0)
  expect_equal(result2, 25.0)
})

test_that("Argument count validation works", {
  double_t <- ffi_double()

  cif <- ffi_cif(double_t, double_t, double_t) # Expects 2 args

  # Too few arguments
  expect_error(
    {
      ffi_call(cif, "test_add_double", 1.0)
    },
    "Expected 2 arguments, got 1"
  )

  # Too many arguments
  expect_error(
    {
      ffi_call(cif, "test_add_double", 1.0, 2.0, 3.0)
    },
    "Expected 2 arguments, got 3"
  )
})

test_that("Type conversion works", {
  load_test_lib()

  double_t <- ffi_double()
  int_t <- ffi_int()

  # Test int to double conversion in function expecting double
  add_fn <- ffi_function("test_add_double", double_t, double_t, double_t)
  result <- add_fn(3L, 2.5) # First arg is integer
  expect_equal(result, 5.5)

  # Test double to int conversion in function expecting int (using whole number)
  add_int_fn <- ffi_function("test_add_int", int_t, int_t, int_t)
  result <- add_int_fn(5.0, 3L) # First arg is double but whole number
  expect_equal(result, 8L)

  # Test that non-integer double is properly rejected
  expect_error(add_int_fn(5.7, 3L), "not integer")
})

test_that("Error handling works", {
  # Invalid symbol name
  expect_error(
    {
      ffi_symbol("nonexistent_function_xyz")
    },
    "Symbol not found"
  )

  # Invalid CIF arguments
  double_t <- ffi_double()
  expect_error(
    {
      ffi_cif("not_a_type")
    },
    "return_type must be an FFIType object"
  )

  expect_error(
    {
      ffi_cif(double_t, "not_a_type")
    },
    "All argument types must be FFIType objects"
  )
})

test_that("Multiple calls work correctly", {
  double_t <- ffi_double()

  square_fn <- ffi_function("test_square", double_t, double_t)

  # Multiple calls should give consistent results
  results <- replicate(10, square_fn(4.0))
  expect_true(all(results == 16.0))

  # Different values
  values <- c(1, 2, 3, 4, 5)
  expected <- values^2
  results <- sapply(values, square_fn)
  expect_equal(results, expected)
})
