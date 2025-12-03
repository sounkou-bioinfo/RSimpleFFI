# Stress Tests for Code Generation - Catch Parsing Errors

library(testthat)
library(RSimpleFFI)

test_that("generated code must be syntactically valid R code", {
  skip_if_not(tcc_available(), "TCC not available")
  
  header <- system.file("extdata", "simple_types.h", package = "RSimpleFFI")
  skip_if(header == "", "Test header not found")
  
  parsed <- ffi_parse_header(header)
  code <- generate_r_bindings(parsed)
  
  tmpfile <- tempfile(fileext = ".R")
  writeLines(code, tmpfile)
  
  expect_no_error(parse(tmpfile))
  expect_no_error(source(tmpfile, local = new.env()))
  
  unlink(tmpfile)
})

test_that("complex_types.h generates valid code", {
  skip_if_not(tcc_available(), "TCC not available")
  
  header <- system.file("extdata", "complex_types.h", package = "RSimpleFFI")
  skip_if(header == "", "Test header not found")
  
  parsed <- ffi_parse_header(header)
  code <- generate_r_bindings(parsed)
  
  tmpfile <- tempfile(fileext = ".R")
  writeLines(code, tmpfile)
  
  expect_no_error(parse(tmpfile))
  expect_no_error(source(tmpfile, local = new.env()))
  
  unlink(tmpfile)
})

test_that("all generated structs have proper syntax", {
  skip_if_not(tcc_available(), "TCC not available")
  
  header <- system.file("extdata", "complex_types.h", package = "RSimpleFFI")
  parsed <- ffi_parse_header(header)
  code <- generate_r_bindings(parsed)
  
  lines <- strsplit(code, "\n")[[1]]
  
  # Check for incomplete field definitions
  for (i in seq_along(lines)) {
    line <- trimws(lines[i])
    
    # Must not have field = with nothing after
    expect_false(grepl("^\\w+\\s*=$", line),
                label = paste("Line", i, "incomplete:", line))
    expect_false(grepl("^\\w+\\s*=\\s*$", line),
                label = paste("Line", i, "empty value:", line))
  }
})

test_that("backticks protect invalid R names", {
  skip_if_not(tcc_available(), "TCC not available")
  
  header <- system.file("extdata", "complex_types.h", package = "RSimpleFFI")
  parsed <- ffi_parse_header(header)
  code <- generate_r_bindings(parsed)
  
  lines <- strsplit(code, "\n")[[1]]
  
  # Check for unprotected _ names
  bare_underscore_assigns <- grep("^_\\w+\\s*<-", lines, value = TRUE)
  expect_equal(length(bare_underscore_assigns), 0,
               label = "All _ names must be backticked")
  
  bare_underscore_fields <- grep("^\\s+_\\w+\\s*=", lines, value = TRUE)
  expect_equal(length(bare_underscore_fields), 0,
               label = "All _ field names must be backticked")
})

test_that("all defines have valid values", {
  skip_if_not(tcc_available(), "TCC not available")
  
  header <- system.file("extdata", "complex_types.h", package = "RSimpleFFI")
  parsed <- ffi_parse_header(header)
  code <- generate_r_bindings(parsed)
  
  tmpfile <- tempfile(fileext = ".R")
  writeLines(code, tmpfile)
  
  expect_no_error(parse(tmpfile))
  expect_no_error({
    env <- new.env()
    eval(parse(tmpfile), envir = env)
  })
  
  unlink(tmpfile)
})
