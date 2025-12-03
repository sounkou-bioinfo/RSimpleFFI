# Tests for Complete Code Generation Workflow

library(testthat)
library(RSimpleFFI)

test_that("generate_r_bindings creates valid R code", {
  skip_if_not(tcc_available(), "TCC not available")
  
  header <- system.file("extdata", "simple_types.h", package = "RSimpleFFI")
  skip_if(header == "", "Test header not found")
  
  parsed <- ffi_parse_header(header)
  code <- generate_r_bindings(parsed)
  
  # Check it's a character vector
  expect_type(code, "character")
  expect_true(nchar(code) > 0)
  
  # Check it contains expected components
  expect_true(grepl("Auto-generated R bindings", code))
  expect_true(grepl("MAX_BUFFER.*1024", code))
  expect_true(grepl("Point.*<-.*ffi_struct", code))
  expect_true(grepl("r_add.*<-.*function", code))
})

test_that("generated code can be sourced and creates functions", {
  skip_if_not(tcc_available(), "TCC not available")
  
  header <- system.file("extdata", "simple_types.h", package = "RSimpleFFI")
  skip_if(header == "", "Test header not found")
  
  parsed <- ffi_parse_header(header)
  code <- generate_r_bindings(parsed)
  
  # Write to temp file and source
  tmpfile <- tempfile(fileext = ".R")
  writeLines(code, tmpfile)
  
  env <- new.env()
  source(tmpfile, local = env)
  
  # Check created objects
  expect_true("MAX_BUFFER" %in% ls(env))
  expect_true("Point" %in% ls(env))
  expect_true("r_add" %in% ls(env))
  expect_true("r_multiply" %in% ls(env))
  expect_true("r_process_point" %in% ls(env))
  
  # Check types
  expect_equal(env$MAX_BUFFER, 1024)
  expect_s3_class(env$Point, "RSimpleFFI::StructType")
  expect_type(env$r_add, "closure")
  
  # Check struct has correct fields
  expect_equal(env$Point@fields, c("x", "y"))
  
  unlink(tmpfile)
})

test_that("generated functions have correct signatures", {
  skip_if_not(tcc_available(), "TCC not available")
  
  header <- system.file("extdata", "simple_types.h", package = "RSimpleFFI")
  parsed <- ffi_parse_header(header)
  
  # Test add function
  add_func <- parsed$functions[parsed$functions$name == "add", ]
  wrapper <- generate_function_wrapper(add_func)
  
  expect_true(grepl("r_add.*<-.*function\\(a, b\\)", wrapper))
  expect_true(grepl("ffi_function.*add.*ffi_int", wrapper))
  
  # Test multiply function
  mult_func <- parsed$functions[parsed$functions$name == "multiply", ]
  wrapper <- generate_function_wrapper(mult_func)
  
  expect_true(grepl("r_multiply.*<-.*function\\(x, y\\)", wrapper))
  expect_true(grepl("ffi_function.*multiply.*ffi_double", wrapper))
})

test_that("generated code includes documentation", {
  skip_if_not(tcc_available(), "TCC not available")
  
  header <- system.file("extdata", "simple_types.h", package = "RSimpleFFI")
  parsed <- ffi_parse_header(header)
  code <- generate_r_bindings(parsed)
  
  # Check header comment
  expect_true(grepl("NOTE:.*symbols.*available", code))
  expect_true(grepl("Type handling:", code))
  
  # Check function docs
  expect_true(grepl("#' Wrapper for C function:", code))
  expect_true(grepl("#' @param", code))
  expect_true(grepl("#' @return", code))
  expect_true(grepl("#' @export", code))
})

test_that("generate_r_bindings can write to file", {
  skip_if_not(tcc_available(), "TCC not available")
  
  header <- system.file("extdata", "simple_types.h", package = "RSimpleFFI")
  parsed <- ffi_parse_header(header)
  
  tmpfile <- tempfile(fileext = ".R")
  result <- generate_r_bindings(parsed, output_file = tmpfile)
  
  # Check file was created
  expect_true(file.exists(tmpfile))
  
  # Check file content matches returned code
  file_content <- paste(readLines(tmpfile), collapse = "\n")
  expect_equal(file_content, result)
  
  unlink(tmpfile)
})

test_that("complex_types.h generates correct array handling", {
  skip_if_not(tcc_available(), "TCC not available")
  
  header <- system.file("extdata", "complex_types.h", package = "RSimpleFFI")
  skip_if(header == "", "Test header not found")
  
  parsed <- ffi_parse_header(header)
  code <- generate_r_bindings(parsed)
  
  # Check multi-dimensional array is generated correctly
  expect_true(grepl("ffi_array_type\\(ffi_array_type\\(ffi_double", code))
  
  # Source and check struct
  tmpfile <- tempfile(fileext = ".R")
  writeLines(code, tmpfile)
  env <- new.env()
  source(tmpfile, local = env)
  
  # Check ArrayStruct exists and has correct structure
  if ("ArrayStruct" %in% ls(env)) {
    expect_s3_class(env$ArrayStruct, "RSimpleFFI::StructType")
  }
  
  unlink(tmpfile)
})

test_that("generated functions use correct FFI types", {
  skip_if_not(tcc_available(), "TCC not available")
  
  header <- system.file("extdata", "simple_types.h", package = "RSimpleFFI")
  parsed <- ffi_parse_header(header)
  
  # Check add: int add(int a, int b)
  add_func <- parsed$functions[parsed$functions$name == "add", ]
  wrapper <- generate_function_wrapper(add_func)
  
  expect_true(grepl('ffi_function\\("add", ffi_int\\(\\), ffi_int\\(\\), ffi_int\\(\\)\\)', wrapper))
  
  # Check multiply: double multiply(double x, double y)
  mult_func <- parsed$functions[parsed$functions$name == "multiply", ]
  wrapper <- generate_function_wrapper(mult_func)
  
  expect_true(grepl('ffi_function\\("multiply", ffi_double\\(\\), ffi_double\\(\\), ffi_double\\(\\)\\)', wrapper))
  
  # Check process_point: void process_point(struct Point *p)
  proc_func <- parsed$functions[parsed$functions$name == "process_point", ]
  wrapper <- generate_function_wrapper(proc_func)
  
  expect_true(grepl('ffi_function\\("process_point", ffi_void\\(\\), ffi_pointer\\(\\)\\)', wrapper))
})
