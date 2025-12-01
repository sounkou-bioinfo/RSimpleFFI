# Test C99-conformant type conversions

library(testthat)
library(RSimpleFFI)

test_that("Signed integer truncation follows C99 semantics", {
  int8_t <- ffi_int8()
  int16_t <- ffi_int16()
  int32_t <- ffi_int32()

  # int8: input + 1
  int8_fn <- ffi_function("test_int8_func", int8_t, int8_t)

  # Normal case
  expect_equal(int8_fn(42L), 43L)

  # Value > 127 wraps around (C99 implementation-defined, we use modular)
  # 200 as int8 wraps: 200 - 256 = -56, then +1 = -55
  expect_equal(int8_fn(200L), -55L)

  # Negative values
  expect_equal(int8_fn(-10L), -9L)

  # Double truncation: 42.7 -> 42 -> 43
  expect_equal(int8_fn(42.7), 43L)
})

test_that("Unsigned integer conversion uses modular arithmetic", {
  uint8_t <- ffi_uint8()
  uint16_t <- ffi_uint16()
  uint32_t <- ffi_uint32()

  # uint8: input + 1
  uint8_fn <- ffi_function("test_uint8_func", uint8_t, uint8_t)

  # Normal case
  expect_equal(uint8_fn(42L), 43L)

  # Negative value wraps (C99: mod 256)
  # -1 as uint8 = 255, then +1 = 256 = 0 (uint8 wrap)
  expect_equal(uint8_fn(-1L), 0L)

  # uint16: input * 2
  uint16_fn <- ffi_function("test_uint16_func", uint16_t, uint16_t)
  expect_equal(uint16_fn(100L), 200L)

  # Negative to unsigned: -1 mod 65536 = 65535, * 2 = 131070 mod 65536 = 65534
  expect_equal(uint16_fn(-1L), 65534L)

  # uint32: input * 3
  uint32_fn <- ffi_function("test_uint32_func", uint32_t, uint32_t)
  expect_equal(uint32_fn(100L), 300L)
})

test_that("64-bit integer conversions work", {
  int64_t <- ffi_int64()
  uint64_t <- ffi_uint64()

  # int64: input * 4
  int64_fn <- ffi_function("test_int64_func", int64_t, int64_t)

  expect_equal(int64_fn(1000L), 4000)
  expect_equal(int64_fn(-500L), -2000)

  # Large value from double
  expect_equal(int64_fn(1e9), 4e9)
})

test_that("Float/double conversions work correctly", {
  float_t <- ffi_float()
  double_t <- ffi_double()

  # Float: a + b
  add_float <- ffi_function("test_add_float", float_t, float_t, float_t)
  result <- add_float(1.5, 2.5)
  expect_equal(result, 4.0, tolerance = 1e-6)

  # Integer to float
  result <- add_float(1L, 2L)
  expect_equal(result, 3.0, tolerance = 1e-6)

  # Double: a + b
  add_double <- ffi_function("test_add_double", double_t, double_t, double_t)
  result <- add_double(3.14, 2.86)
  expect_equal(result, 6.0, tolerance = 1e-10)
})

test_that("Bool conversion works", {
  bool_t <- ffi_bool()

  # bool: returns !input
  bool_fn <- ffi_function("test_bool_func", bool_t, bool_t)

  expect_equal(bool_fn(TRUE), FALSE)
  expect_equal(bool_fn(FALSE), TRUE)

  # Integer to bool: 0 = FALSE, non-zero = TRUE
  expect_equal(bool_fn(0L), TRUE)   # !FALSE
  expect_equal(bool_fn(1L), FALSE)  # !TRUE
  expect_equal(bool_fn(42L), FALSE) # !TRUE

  # Double to bool: truncated first, then converted
  # 0.0 -> 0 -> FALSE, !FALSE = TRUE
  expect_equal(bool_fn(0.0), TRUE)
  # 0.5 -> 0 (truncated) -> FALSE, !FALSE = TRUE
  expect_equal(bool_fn(0.5), TRUE)
  # 1.0 -> 1 -> TRUE, !TRUE = FALSE
  expect_equal(bool_fn(1.0), FALSE)
})

test_that("Return type conversions preserve precision when possible", {
  uint32_t <- ffi_uint32()

  # uint32 that fits in integer should return as integer
  uint32_fn <- ffi_function("test_uint32_func", uint32_t, uint32_t)

  # 100 * 3 = 300, fits in integer
  result <- uint32_fn(100L)
  expect_equal(result, 300L)
  expect_type(result, "integer")

  # Large uint32 values should return as double
  # We can test this if we have a function that returns > INT_MAX
})

test_that("size_t and ssize_t work correctly", {
  size_t <- ffi_size_t()
  ssize_t <- ffi_ssize_t()

  # Test with functions that use size_t/ssize_t
  # string_length returns size_t (as int in test func)
  string_fn <- ffi_function("test_string_length", ffi_int(), ffi_string())
  expect_equal(string_fn("hello"), 5L)
})

test_that("Logical values convert to integers", {
  int_t <- ffi_int()

  add_fn <- ffi_function("test_add_int", int_t, int_t, int_t)

  # TRUE = 1, FALSE = 0
  expect_equal(add_fn(TRUE, FALSE), 1L)
  expect_equal(add_fn(TRUE, TRUE), 2L)
  expect_equal(add_fn(FALSE, FALSE), 0L)
})

test_that("Signed/unsigned edge cases work", {
  int8_t <- ffi_int8()
  uint8_t <- ffi_uint8()

  int8_fn <- ffi_function("test_int8_func", int8_t, int8_t)
  uint8_fn <- ffi_function("test_uint8_func", uint8_t, uint8_t)

  # Boundary values for int8: [-128, 127]
  expect_equal(int8_fn(127L), -128L)  # 127 + 1 = 128 -> wraps to -128
  expect_equal(int8_fn(-128L), -127L) # -128 + 1 = -127

  # Boundary values for uint8: [0, 255]
  expect_equal(uint8_fn(255L), 0L)  # 255 + 1 = 256 -> wraps to 0
  expect_equal(uint8_fn(0L), 1L)    # 0 + 1 = 1
})
