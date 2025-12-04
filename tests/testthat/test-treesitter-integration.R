test_that("tree-sitter extracts same structs as regex parser", {
  skip_if_not_installed("treesitter")
  skip_if_not_installed("treesitter.c")
  
  header <- system.file("extdata", "simple_types.h", package = "RSimpleFFI")
  
  # Parse with both methods
  result_ts <- ffi_parse_header(header, use_treesitter = TRUE)
  result_regex <- ffi_parse_header(header, use_treesitter = FALSE)
  
  expect_equal(result_ts$parser, "treesitter")
  expect_equal(result_regex$parser, "regex")
  
  # Compare struct names
  expect_setequal(names(result_ts$structs), names(result_regex$structs))
  
  # Compare Point struct fields
  ts_point_fields <- sapply(result_ts$structs$Point, function(f) f$name)
  regex_point_fields <- sapply(result_regex$structs$Point, function(f) f$name)
  expect_setequal(ts_point_fields, regex_point_fields)
  
  # Compare field types
  ts_x_type <- result_ts$structs$Point[[which(ts_point_fields == "x")]]$type
  regex_x_type <- result_regex$structs$Point[[which(regex_point_fields == "x")]]$type
  expect_equal(ts_x_type, regex_x_type)
})

test_that("tree-sitter extracts functions correctly", {
  skip_if_not_installed("treesitter")
  skip_if_not_installed("treesitter.c")
  
  header <- system.file("extdata", "simple_types.h", package = "RSimpleFFI")
  result <- ffi_parse_header(header, use_treesitter = TRUE)
  
  expect_true(nrow(result$functions) > 0)
  expect_true("add" %in% result$functions$name)
  expect_true("multiply" %in% result$functions$name)
  expect_true("process_point" %in% result$functions$name)
  
  # Check function details
  add_fn <- result$functions[result$functions$name == "add", ]
  expect_equal(add_fn$return_type, "int")
  expect_match(add_fn$params, "int.*int")
})

test_that("tree-sitter extracts enums correctly", {
  skip_if_not_installed("treesitter")
  skip_if_not_installed("treesitter.c")
  
  header <- system.file("extdata", "enums_unions.h", package = "RSimpleFFI")
  result <- ffi_parse_header(header, use_treesitter = TRUE)
  
  # Check Color enum
  expect_true("Color" %in% names(result$enums))
  color_enum <- result$enums$Color
  expect_equal(unname(color_enum["RED"]), 0L)
  expect_equal(unname(color_enum["GREEN"]), 1L)
  expect_equal(unname(color_enum["BLUE"]), 2L)
})

test_that("tree-sitter extracts unions correctly", {
  skip_if_not_installed("treesitter")
  skip_if_not_installed("treesitter.c")
  
  header <- system.file("extdata", "enums_unions.h", package = "RSimpleFFI")
  result <- ffi_parse_header(header, use_treesitter = TRUE)
  
  # Check Value union
  expect_true("Value" %in% names(result$unions))
  value_union <- result$unions$Value
  field_names <- sapply(value_union, function(f) f$name)
  expect_true("as_int" %in% field_names)
  expect_true("as_float" %in% field_names)
  expect_true("as_double" %in% field_names)
})

test_that("tree-sitter extracts typedefs correctly", {
  skip_if_not_installed("treesitter")
  skip_if_not_installed("treesitter.c")
  
  header <- system.file("extdata", "typedef_structs.h", package = "RSimpleFFI")
  result <- ffi_parse_header(header, use_treesitter = TRUE)
  
  expect_true(length(result$typedefs) > 0)
})

test_that("tree-sitter handles complex types", {
  skip_if_not_installed("treesitter")
  skip_if_not_installed("treesitter.c")
  
  header <- system.file("extdata", "complex_types.h", package = "RSimpleFFI")
  result <- ffi_parse_header(header, use_treesitter = TRUE)
  
  # Should parse without errors
  expect_s3_class(result, "parsed_header")
  expect_equal(result$parser, "treesitter")
})

test_that("tree-sitter and regex produce compatible output for codegen", {
  skip_if_not_installed("treesitter")
  skip_if_not_installed("treesitter.c")
  
  header <- system.file("extdata", "simple_types.h", package = "RSimpleFFI")
  
  # Generate code with both parsers
  result_ts <- ffi_parse_header(header, use_treesitter = TRUE)
  result_regex <- ffi_parse_header(header, use_treesitter = FALSE)
  
  # Both should generate valid R code
  code_ts <- generate_r_bindings(result_ts)
  code_regex <- generate_r_bindings(result_regex)
  
  expect_true(length(code_ts) > 0)
  expect_true(length(code_regex) > 0)
  
  # Both should have struct definitions
  expect_true(any(grepl("Point.*<-.*ffi_struct", code_ts)))
  expect_true(any(grepl("Point.*<-.*ffi_struct", code_regex)))
})
