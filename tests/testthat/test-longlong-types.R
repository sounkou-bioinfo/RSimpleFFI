# Tests for long long type support

test_that("ffi_longlong type exists and has correct size", {
  ll_type <- ffi_longlong()
  expect_s3_class(ll_type, "RSimpleFFI::FFIType")
  expect_equal(ffi_sizeof(ll_type), 8L) # 64-bit
})

test_that("ffi_ulonglong type exists and has correct size", {
  ull_type <- ffi_ulonglong()
  expect_s3_class(ull_type, "RSimpleFFI::FFIType")
  expect_equal(ffi_sizeof(ull_type), 8L) # 64-bit
})

test_that("long long FFI calls work", {
  skip_if_not(exists("ffi_symbol"))

  sym <- ffi_symbol("test_longlong_func")
  cif <- ffi_cif(ffi_longlong(), ffi_longlong())

  # Test with small value
  result <- ffi_call(cif, sym, 10)
  expect_equal(result, 20) # a * 2

  # Test with larger value (but still fits in double precision)
  result2 <- ffi_call(cif, sym, 1000000000)
  expect_equal(result2, 2000000000)
})

test_that("unsigned long long FFI calls work", {
  skip_if_not(exists("ffi_symbol"))

  sym <- ffi_symbol("test_ulonglong_func")
  cif <- ffi_cif(ffi_ulonglong(), ffi_ulonglong())

  # Test with value
  result <- ffi_call(cif, sym, 10)
  expect_equal(result, 30) # a * 3
})

test_that("header parser maps long long correctly", {
  # Test that generate_function_wrapper handles long long
  func_def <- data.frame(
    name = "test_func",
    return_type = "long long",
    params = "long long x",
    stringsAsFactors = FALSE
  )
  # Add param_list structure (required by tree-sitter based parser)
  func_def$param_list <- list(
    test_func = list(
      list(type = "long long", name = "x", is_variadic = FALSE)
    )
  )

  wrapper <- generate_function_wrapper(func_def)
  expect_true(any(grepl("ffi_longlong", wrapper)))
})

test_that("header parser maps unsigned long long correctly", {
  func_def <- data.frame(
    name = "test_func",
    return_type = "unsigned long long",
    params = "unsigned long long x",
    stringsAsFactors = FALSE
  )
  # Add param_list structure (required by tree-sitter based parser)
  func_def$param_list <- list(
    test_func = list(
      list(type = "unsigned long long", name = "x", is_variadic = FALSE)
    )
  )

  wrapper <- generate_function_wrapper(func_def)
  expect_true(any(grepl("ffi_ulonglong", wrapper)))
})
