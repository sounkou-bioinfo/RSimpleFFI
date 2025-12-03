test_that("C literal suffixes are properly stripped", {
  # Create test header with various C literal suffixes
  test_header <- tempfile(fileext = ".h")
  cat("
#define INT64_MAX 9223372036854775807LL
#define UINT64_MAX 0xffffffffffffffffULL
#define INT32_MAX 2147483647
#define SMALL_INT 100U
#define FLOAT_VAL 3.14f
#define DOUBLE_VAL 2.71828
#define EXP_VAL 1.5e10f
#define HEX_VAL 0xDEADBEEFUL
#define HEX_WITH_F 0xCAFEF00DUL
", file = test_header)
  
  parsed <- ffi_parse_header(test_header)
  code <- generate_r_bindings(parsed)
  
  # Generated code should not contain numeric literals with LL/ULL/f/F suffixes
  # (but strings and identifiers can contain these letter combinations)
  expect_false(grepl("[0-9]+LL\\b", code))  # No number followed by LL
  expect_false(grepl("[0-9]+ULL\\b", code)) # No number followed by ULL
  expect_false(grepl("[0-9]f\\s*$", code, perl = TRUE))  # No trailing f after number
  expect_false(grepl("[0-9]F\\s*$", code, perl = TRUE))  # No trailing F after number
  
  # Code should parse without errors
  tmpfile <- tempfile(fileext = ".R")
  writeLines(code, tmpfile)
  expect_no_error(parse(tmpfile))
  
  # Code should source without errors
  env <- new.env()
  expect_no_error(source(tmpfile, local = env))
  
  # Check values are correct
  expect_equal(env$INT32_MAX, 2147483647)
  expect_equal(env$SMALL_INT, 100)
  expect_equal(env$FLOAT_VAL, 3.14)
  expect_equal(env$DOUBLE_VAL, 2.71828)
  expect_equal(env$EXP_VAL, 1.5e10)
  
  # Check hex values parse correctly
  expect_equal(env$HEX_VAL, 0xDEADBEEF)
  expect_equal(env$HEX_WITH_F, 0xCAFEF00D)
  
  # INT64_MAX should be numeric (too large for R integer)
  expect_true(is.numeric(env$INT64_MAX))
  expect_equal(env$INT64_MAX, 9223372036854775807)
  
  # UINT64_MAX should parse as hex
  expect_true(is.numeric(env$UINT64_MAX))
})

test_that("macOS-style stdint.h defines are handled", {
  # Simulate macOS stdint.h which uses LL suffixes
  test_header <- tempfile(fileext = ".h")
  cat("
#define INT8_MIN   (-128)
#define INT16_MIN  (-32768)
#define INT32_MIN  (-2147483647 - 1)
#define INT64_MIN  (-9223372036854775807LL - 1)
#define INT8_MAX   127
#define INT16_MAX  32767
#define INT32_MAX  2147483647
#define INT64_MAX  9223372036854775807LL
#define UINT8_MAX  255
#define UINT16_MAX 65535
#define UINT32_MAX 4294967295U
#define UINT64_MAX 18446744073709551615ULL
", file = test_header)
  
  parsed <- ffi_parse_header(test_header)
  code <- generate_r_bindings(parsed)
  
  tmpfile <- tempfile(fileext = ".R")
  writeLines(code, tmpfile)
  
  # Should parse and source without errors (the macOS bug)
  expect_no_error(parse(tmpfile))
  env <- new.env()
  expect_no_error(source(tmpfile, local = env))
  
  # Verify values
  expect_equal(env$INT8_MAX, 127)
  expect_equal(env$INT16_MAX, 32767)
  expect_equal(env$INT32_MAX, 2147483647L)
  expect_equal(env$UINT32_MAX, 4294967295)
})
