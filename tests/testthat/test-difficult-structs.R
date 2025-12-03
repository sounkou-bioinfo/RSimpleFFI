# Tests for Difficult Struct Cases

library(testthat)
library(RSimpleFFI)

test_that("FILE* and opaque pointers generate valid code", {
  skip_if_not(tcc_available(), "TCC not available")
  
  header <- system.file("extdata", "difficult_structs.h", package = "RSimpleFFI")
  skip_if(header == "", "difficult_structs.h not found")
  
  parsed <- ffi_parse_header(header)
  code <- generate_r_bindings(parsed)
  
  tmpfile <- tempfile(fileext = ".R")
  writeLines(code, tmpfile)
  
  # Must parse without errors
  expect_no_error(parse(tmpfile))
  
  # Must source without errors
  env <- new.env()
  expect_no_error(source(tmpfile, local = env))
  
  # Check that FILE* functions are generated
  expect_true("r_open_file" %in% ls(env))
  expect_true("r_close_file" %in% ls(env))
  
  unlink(tmpfile)
})

test_that("recursive structs generate valid definitions", {
  skip_if_not(tcc_available(), "TCC not available")
  
  header <- system.file("extdata", "difficult_structs.h", package = "RSimpleFFI")
  parsed <- ffi_parse_header(header)
  code <- generate_r_bindings(parsed)
  
  tmpfile <- tempfile(fileext = ".R")
  writeLines(code, tmpfile)
  
  env <- new.env()
  expect_no_error(source(tmpfile, local = env))
  
  # Check recursive structs exist
  if ("Node" %in% ls(env)) {
    expect_s3_class(env$Node, "RSimpleFFI::StructType")
    # Should have fields: value, next, prev
    expect_true("value" %in% env$Node@fields)
  }
  
  if ("Tree" %in% ls(env)) {
    expect_s3_class(env$Tree, "RSimpleFFI::StructType")
  }
  
  unlink(tmpfile)
})

test_that("nested structs generate complete definitions", {
  skip_if_not(tcc_available(), "TCC not available")
  
  header <- system.file("extdata", "difficult_structs.h", package = "RSimpleFFI")
  parsed <- ffi_parse_header(header)
  code <- generate_r_bindings(parsed)
  
  tmpfile <- tempfile(fileext = ".R")
  writeLines(code, tmpfile)
  
  env <- new.env()
  expect_no_error(source(tmpfile, local = env))
  
  # Check nested structs
  if ("Outer" %in% ls(env)) {
    expect_s3_class(env$Outer, "RSimpleFFI::StructType")
  }
  
  if ("Middle" %in% ls(env)) {
    expect_s3_class(env$Middle, "RSimpleFFI::StructType")
  }
  
  if ("Inner" %in% ls(env)) {
    expect_s3_class(env$Inner, "RSimpleFFI::StructType")
  }
  
  unlink(tmpfile)
})

test_that("function pointer types are handled", {
  skip_if_not(tcc_available(), "TCC not available")
  
  header <- system.file("extdata", "difficult_structs.h", package = "RSimpleFFI")
  parsed <- ffi_parse_header(header)
  code <- generate_r_bindings(parsed)
  
  # Should not have syntax errors even with function pointers
  tmpfile <- tempfile(fileext = ".R")
  writeLines(code, tmpfile)
  
  expect_no_error(parse(tmpfile))
  expect_no_error(source(tmpfile, local = new.env()))
  
  unlink(tmpfile)
})

test_that("structs with arrays generate correctly", {
  skip_if_not(tcc_available(), "TCC not available")
  
  header <- system.file("extdata", "difficult_structs.h", package = "RSimpleFFI")
  parsed <- ffi_parse_header(header)
  code <- generate_r_bindings(parsed)
  
  tmpfile <- tempfile(fileext = ".R")
  writeLines(code, tmpfile)
  
  env <- new.env()
  expect_no_error(source(tmpfile, local = env))
  
  # Check struct with array of structs
  if ("Polygon" %in% ls(env)) {
    expect_s3_class(env$Polygon, "RSimpleFFI::StructType")
    expect_true("vertices" %in% env$Polygon@fields)
  }
  
  unlink(tmpfile)
})

test_that("typedef structs are handled", {
  skip_if_not(tcc_available(), "TCC not available")
  
  header <- system.file("extdata", "difficult_structs.h", package = "RSimpleFFI")
  parsed <- ffi_parse_header(header)
  code <- generate_r_bindings(parsed)
  
  tmpfile <- tempfile(fileext = ".R")
  writeLines(code, tmpfile)
  
  env <- new.env()
  expect_no_error(source(tmpfile, local = env))
  
  # Check typedef'd struct
  if ("LinkedList" %in% ls(env)) {
    expect_s3_class(env$LinkedList, "RSimpleFFI::StructType")
  }
  
  unlink(tmpfile)
})

test_that("complex function signatures generate valid wrappers", {
  skip_if_not(tcc_available(), "TCC not available")
  
  header <- system.file("extdata", "difficult_structs.h", package = "RSimpleFFI")
  parsed <- ffi_parse_header(header)
  code <- generate_r_bindings(parsed)
  
  tmpfile <- tempfile(fileext = ".R")
  writeLines(code, tmpfile)
  
  env <- new.env()
  expect_no_error(source(tmpfile, local = env))
  
  # Check various function types were generated
  if ("r_create_node" %in% ls(env)) {
    expect_type(env$r_create_node, "closure")
  }
  
  if ("r_insert_node" %in% ls(env)) {
    expect_type(env$r_insert_node, "closure")
  }
  
  unlink(tmpfile)
})

test_that("all generated code from difficult_structs.h is valid", {
  skip_if_not(tcc_available(), "TCC not available")
  
  header <- system.file("extdata", "difficult_structs.h", package = "RSimpleFFI")
  parsed <- ffi_parse_header(header)
  code <- generate_r_bindings(parsed)
  
  # Write and check
  tmpfile <- tempfile(fileext = ".R")
  writeLines(code, tmpfile)
  
  # This is the ultimate test - must parse
  parsed_code <- parse(tmpfile)
  expect_true(length(parsed_code) > 0)
  
  # Must source
  env <- new.env()
  expect_no_error(eval(parsed_code, envir = env))
  
  # Check we got something
  objects <- ls(env)
  expect_true(length(objects) > 0)
  
  unlink(tmpfile)
})

test_that("no incomplete struct definitions in difficult cases", {
  skip_if_not(tcc_available(), "TCC not available")
  
  header <- system.file("extdata", "difficult_structs.h", package = "RSimpleFFI")
  parsed <- ffi_parse_header(header)
  code <- generate_r_bindings(parsed)
  
  lines <- strsplit(code, "\n")[[1]]
  
  # Check for common syntax errors
  for (i in seq_along(lines)) {
    line <- trimws(lines[i])
    
    # No incomplete assignments
    expect_false(grepl("^\\w+\\s*=$", line),
                label = paste("Line", i, "incomplete"))
    
    # No lines with just =
    expect_false(grepl("^\\s*=\\s*$", line),
                label = paste("Line", i, "orphan equals"))
  }
  
  # Check struct definitions are balanced
  struct_starts <- grep("<- ffi_struct\\(", lines)
  if (length(struct_starts) > 0) {
    for (start in struct_starts) {
      # Find matching closing paren
      paren_count <- 1
      found_close <- FALSE
      for (j in (start + 1):length(lines)) {
        if (j > length(lines)) break
        paren_count <- paren_count + nchar(gsub("[^(]", "", lines[j]))
        paren_count <- paren_count - nchar(gsub("[^)]", "", lines[j]))
        if (paren_count == 0) {
          found_close <- TRUE
          break
        }
      }
      expect_true(found_close,
                  label = paste("Struct at line", start, "must close"))
    }
  }
})
