test_that("tree-sitter parser can parse simple struct", {
  skip_if_not_installed("treesitter")
  skip_if_not_installed("treesitter.c")
  
  # Create a temporary header file
  tmp_header <- tempfile(fileext = ".h")
  on.exit(unlink(tmp_header))
  
  writeLines(c(
    "#define MAX_SIZE 100",
    "",
    "struct Point {",
    "    int x[MAX_SIZE];",
    "    int y;",
    "};"
  ), tmp_header)
  
  # Parse with tree-sitter
  result <- ffi_parse_header_ts(tmp_header, use_treesitter = TRUE)
  
  expect_s3_class(result, "parsed_header")
  expect_equal(result$parser, "treesitter")
  expect_true("Point" %in% names(result$structs))
  
  # After TCC preprocessing, MAX_SIZE should be expanded to 100
  point_fields <- result$structs$Point
  expect_true("x" %in% names(point_fields))
  expect_true("y" %in% names(point_fields))
  
  # Check that x is an array (should be "int[100]" after preprocessing)
  expect_match(point_fields$x, "int\\[100\\]")
  expect_equal(point_fields$y, "int")
})

test_that("tree-sitter parser falls back gracefully", {
  tmp_header <- tempfile(fileext = ".h")
  on.exit(unlink(tmp_header))
  
  writeLines(c(
    "struct Simple {",
    "    int value;",
    "};"
  ), tmp_header)
  
  # Should work whether or not tree-sitter is available
  result <- ffi_parse_header_ts(tmp_header, use_treesitter = FALSE)
  
  expect_s3_class(result, "parsed_header")
  expect_true("Simple" %in% names(result$structs))
})

test_that("tree-sitter handles multi-dimensional arrays", {
  skip_if_not_installed("treesitter")
  skip_if_not_installed("treesitter.c")
  
  tmp_header <- tempfile(fileext = ".h")
  on.exit(unlink(tmp_header))
  
  writeLines(c(
    "struct Matrix {",
    "    int data[3][4];",
    "    int rows;",
    "};"
  ), tmp_header)
  
  result <- ffi_parse_header_ts(tmp_header, use_treesitter = TRUE)
  
  expect_true("Matrix" %in% names(result$structs))
  matrix_fields <- result$structs$Matrix
  
  expect_true("data" %in% names(matrix_fields))
  expect_match(matrix_fields$data, "int\\[3\\]\\[4\\]")
})
