# Tests for Header Parsing and Code Generation

library(testthat)
library(RSimpleFFI)

test_that("ffi_parse_header works on simple header", {
  skip_if_not(tcc_available(), "TCC not available")
  
  header <- system.file("extdata", "simple_types.h", package = "RSimpleFFI")
  skip_if(header == "", "Test header not found")
  
  result <- ffi_parse_header(header)
  
  expect_s3_class(result, "parsed_header")
  expect_named(result, c("file", "defines", "structs", "functions"))
  
  # Check defines
  expect_type(result$defines, "list")
  expect_true("MAX_BUFFER" %in% names(result$defines))
  expect_equal(result$defines$MAX_BUFFER, "1024")
  
  # Check structs
  expect_type(result$structs, "list")
  expect_true("Point" %in% names(result$structs))
  
  # Check functions
  expect_s3_class(result$functions, "data.frame")
  expect_true("add" %in% result$functions$name)
})

test_that("ffi_parse_header works with typedef structs", {
  skip_if_not(tcc_available(), "TCC not available")
  
  header <- system.file("extdata", "typedef_structs.h", package = "RSimpleFFI")
  skip_if(header == "", "Test header not found")
  
  result <- ffi_parse_header(header)
  
  # Check typedef structs are extracted
  expect_true("Vector3D" %in% names(result$structs))
  expect_true("Color" %in% names(result$structs))
  expect_true("Dimensions" %in% names(result$structs))
  
  # Check Vector3D fields
  vector3d <- result$structs$Vector3D
  expect_length(vector3d, 3)
  expect_equal(vector3d[[1]]$name, "x")
  expect_equal(vector3d[[2]]$name, "y")
  expect_equal(vector3d[[3]]$name, "z")
})

test_that("ffi_parse_header works with includes", {
  skip_if_not(tcc_available(), "TCC not available")
  
  header <- system.file("extdata", "with_includes.h", package = "RSimpleFFI")
  inc_dir <- system.file("extdata", "includes", package = "RSimpleFFI")
  skip_if(header == "" || inc_dir == "", "Test files not found")
  
  result <- ffi_parse_header(header, includes = inc_dir)
  
  # Check defines from included file
  expect_true("BUFFER_SIZE" %in% names(result$defines))
  expect_equal(result$defines$BUFFER_SIZE, "4096")
  
  # Check structs from included file  
  expect_true("Event" %in% names(result$structs))
  expect_true("Buffer" %in% names(result$structs))
  expect_true("EventQueue" %in% names(result$structs))
})

test_that("generate_struct_definition creates valid R code", {
  struct_def <- list(
    list(type = "int", name = "x"),
    list(type = "int", name = "y")
  )
  
  code <- generate_struct_definition("Point", struct_def)
  
  expect_type(code, "character")
  expect_match(code, "Point <- ffi_struct")
  expect_match(code, "x = ffi_int()")
  expect_match(code, "y = ffi_int()")
})

test_that("generate_struct_definition handles various types", {
  struct_def <- list(
    list(type = "double", name = "value"),
    list(type = "char *", name = "name"),
    list(type = "uint32_t", name = "id")
  )
  
  code <- generate_struct_definition("MyStruct", struct_def)
  
  expect_match(code, "value = ffi_double()")
  expect_match(code, "name = ffi_pointer()")
  expect_match(code, "id = ffi_uint32()")
})

test_that("generate_function_wrapper creates valid R code", {
  func_def <- data.frame(
    name = "add",
    return_type = "int",
    params = "int a, int b",
    full_declaration = "int add(int a, int b);",
    stringsAsFactors = FALSE
  )
  
  code <- generate_function_wrapper(func_def)
  
  expect_type(code, "character")
  expect_match(code, "r_add <- function\\(a, b, lib = NULL\\)")
  expect_match(code, "#' Wrapper for add")
  expect_match(code, "#' @export")
})

test_that("generate_r_bindings produces complete code", {
  skip_if_not(tcc_available(), "TCC not available")
  
  header <- system.file("extdata", "simple_types.h", package = "RSimpleFFI")
  skip_if(header == "", "Test header not found")
  
  parsed <- ffi_parse_header(header)
  code <- generate_r_bindings(parsed)
  
  expect_type(code, "character")
  expect_true(length(code) > 0)
  
  # Check for key elements
  code_str <- paste(code, collapse = "\n")
  expect_match(code_str, "# Auto-generated")
  expect_match(code_str, "MAX_BUFFER")
  expect_match(code_str, "Point <- ffi_struct")
  expect_match(code_str, "r_add <- function")
})

test_that("generate_r_bindings can write to file", {
  skip_if_not(tcc_available(), "TCC not available")
  
  header <- system.file("extdata", "simple_types.h", package = "RSimpleFFI")
  skip_if(header == "", "Test header not found")
  
  parsed <- ffi_parse_header(header)
  tmp_file <- tempfile(fileext = ".R")
  on.exit(unlink(tmp_file))
  
  code <- generate_r_bindings(parsed, output_file = tmp_file)
  
  expect_true(file.exists(tmp_file))
  file_content <- readLines(tmp_file)
  expect_true(length(file_content) > 0)
})

test_that("generated struct code is executable", {
  struct_def <- list(
    list(type = "int", name = "x"),
    list(type = "double", name = "y")
  )
  
  code <- generate_struct_definition("TestStruct", struct_def)
  
  # Parse and evaluate the code
  expect_no_error({
    eval(parse(text = code))
  })
  
  # Check the created struct
  expect_true(exists("TestStruct"))
  expect_s7_class(TestStruct, RSimpleFFI::StructType)
})
