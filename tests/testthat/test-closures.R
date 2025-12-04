# Tests for FFI Closure API

test_that("closures are supported", {
  expect_true(ffi_closures_supported())
})

test_that("basic closure creation works", {
  # Create a simple R function
  double_it <- function(x) {
    as.integer(x * 2L)
  }

  # Wrap it as a C callback: int (*)(int)
  closure <- ffi_closure(double_it, ffi_int(), ffi_int())

  expect_s3_class(closure, "RSimpleFFI::FFIClosure")
  expect_true(is.function(closure@r_function))
  expect_true(inherits(closure@func_ptr, "externalptr"))
})

test_that("closure can be called via test_callback", {
  # R function: adds 10 to input
  add_ten <- function(x) {
    as.integer(x + 10L)
  }

  # Create closure with signature: int (*)(int)
  closure <- ffi_closure(add_ten, ffi_int(), ffi_int())
  callback_ptr <- ffi_closure_pointer(closure)

  # Call test_callback(func, value) which returns func(value)
  test_callback_fn <- ffi_function(
    "test_callback",
    ffi_int(), # return type
    ffi_pointer(), # callback function pointer
    ffi_int() # value to pass to callback
  )

  result <- test_callback_fn(callback_ptr, 5L)
  expect_equal(result, 15L) # 5 + 10 = 15

  result2 <- test_callback_fn(callback_ptr, 100L)
  expect_equal(result2, 110L) # 100 + 10 = 110
})

test_that("double callback works", {
  # R function: squares input
  square <- function(x) {
    x * x
  }

  # Create closure with signature: double (*)(double)
  closure <- ffi_closure(square, ffi_double(), ffi_double())
  callback_ptr <- ffi_closure_pointer(closure)

  # Call test_double_callback(func, value)
  test_double_callback_fn <- ffi_function(
    "test_double_callback",
    ffi_double(),
    ffi_pointer(),
    ffi_double()
  )

  result <- test_double_callback_fn(callback_ptr, 3.0)
  expect_equal(result, 9.0)

  result2 <- test_double_callback_fn(callback_ptr, 2.5)
  expect_equal(result2, 6.25)
})

test_that("closure with multiple arguments works", {
  # Comparison function for finding max
  compare <- function(a, b) {
    as.integer(a - b) # positive if a > b
  }

  # Create closure: int (*)(int, int)
  cmp_closure <- ffi_closure(compare, ffi_int(), ffi_int(), ffi_int())
  cmp_ptr <- ffi_closure_pointer(cmp_closure)

  # Set up array of integers
  arr <- as.integer(c(3, 7, 2, 9, 1))
  arr_ptr <- ffi_alloc(ffi_int(), length(arr))
  ffi_fill_typed_buffer(arr_ptr, arr, ffi_int())

  # Call test_find_max(arr, len, cmp)
  find_max_fn <- ffi_function(
    "test_find_max",
    ffi_int(),
    ffi_pointer(),
    ffi_int(),
    ffi_pointer()
  )

  result <- find_max_fn(arr_ptr, length(arr), cmp_ptr)
  expect_equal(result, 9L)
})

test_that("transform_sum with closure works", {
  # Transform function: returns absolute value
  abs_val <- function(x) {
    abs(x)
  }

  # Create closure: double (*)(double)
  transform_closure <- ffi_closure(abs_val, ffi_double(), ffi_double())
  transform_ptr <- ffi_closure_pointer(transform_closure)

  # Set up array with mixed signs
  arr <- c(-1.0, 2.0, -3.0, 4.0, -5.0)
  arr_ptr <- ffi_alloc(ffi_double(), length(arr))
  ffi_fill_typed_buffer(arr_ptr, arr, ffi_double())

  # Call test_transform_sum(arr, len, transform)
  transform_sum_fn <- ffi_function(
    "test_transform_sum",
    ffi_double(),
    ffi_pointer(),
    ffi_int(),
    ffi_pointer()
  )

  result <- transform_sum_fn(arr_ptr, length(arr), transform_ptr)
  expect_equal(result, 15.0) # |−1| + |2| + |−3| + |4| + |−5| = 15
})

test_that("void callback works", {
  # Track calls with a closure environment
  call_log <- new.env()
  call_log$values <- integer(0)

  log_value <- function(x) {
    call_log$values <- c(call_log$values, x)
    invisible(NULL)
  }

  # Create closure: void (*)(int)
  void_closure <- ffi_closure(log_value, ffi_void(), ffi_int())
  void_ptr <- ffi_closure_pointer(void_closure)

  # Call test_foreach(start, end, callback)
  foreach_fn <- ffi_function(
    "test_foreach",
    ffi_void(),
    ffi_int(),
    ffi_int(),
    ffi_pointer()
  )

  foreach_fn(1L, 5L, void_ptr)

  expect_equal(call_log$values, c(1L, 2L, 3L, 4L))
})

test_that("closure keeps R function alive", {
  # Create closure and let original function go out of scope
  create_closure <- function() {
    inner_fn <- function(x) as.integer(x + 1L)
    ffi_closure(inner_fn, ffi_int(), ffi_int())
  }

  closure <- create_closure()
  gc() # Try to trigger GC

  # Closure should still work
  callback_ptr <- ffi_closure_pointer(closure)
  test_callback_fn <- ffi_function(
    "test_callback",
    ffi_int(),
    ffi_pointer(),
    ffi_int()
  )

  result <- test_callback_fn(callback_ptr, 41L)
  expect_equal(result, 42L)
})

test_that("ffi_closure_pointer returns correct pointer", {
  fn <- function(x) as.integer(x)
  closure <- ffi_closure(fn, ffi_int(), ffi_int())

  ptr1 <- ffi_closure_pointer(closure)
  ptr2 <- closure@func_ptr

  # Both should return the same pointer
  expect_identical(ptr1, ptr2)
})

test_that("closure validates input", {
  expect_error(ffi_closure("not a function", ffi_int(), ffi_int()))
  expect_error(ffi_closure_pointer("not a closure"))
})
