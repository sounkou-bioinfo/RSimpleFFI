test_that("tree-sitter parser can parse simple struct", {
  skip_if_not_installed("treesitter")
  skip_if_not_installed("treesitter.c")

  # Create a temporary header file
  tmp_header <- tempfile(fileext = ".h")
  on.exit(unlink(tmp_header))

  writeLines(
    c(
      "#define MAX_SIZE 100",
      "",
      "struct Point {",
      "    int x[MAX_SIZE];",
      "    int y;",
      "};"
    ),
    tmp_header
  )

  # Parse with tree-sitter
  result <- ffi_parse_header(tmp_header)

  expect_s3_class(result, "parsed_header")
  expect_equal(result$parser, "treesitter")
  expect_true("Point" %in% names(result$structs))

  # After TCC preprocessing, MAX_SIZE should be expanded to 100
  point_fields <- result$structs$Point
  field_names <- sapply(point_fields, function(f) f$name)
  expect_true("x" %in% field_names)
  expect_true("y" %in% field_names)

  # Check that x is an array (should be "int[100]" after preprocessing)
  x_field <- point_fields[[which(field_names == "x")]]
  y_field <- point_fields[[which(field_names == "y")]]
  expect_match(x_field$type, "int\\[100\\]")
  expect_equal(y_field$type, "int")
})

test_that("tree-sitter parser falls back gracefully", {
  tmp_header <- tempfile(fileext = ".h")
  on.exit(unlink(tmp_header))

  writeLines(
    c(
      "struct Simple {",
      "    int value;",
      "};"
    ),
    tmp_header
  )

  # Parse with tree-sitter
  result <- ffi_parse_header(tmp_header)

  expect_s3_class(result, "parsed_header")
  expect_true("Simple" %in% names(result$structs))
})

test_that("tree-sitter handles multi-dimensional arrays", {
  skip_if_not_installed("treesitter")
  skip_if_not_installed("treesitter.c")

  tmp_header <- tempfile(fileext = ".h")
  on.exit(unlink(tmp_header))

  writeLines(
    c(
      "struct Matrix {",
      "    int data[3][4];",
      "    int rows;",
      "};"
    ),
    tmp_header
  )

  result <- ffi_parse_header(tmp_header)

  expect_true("Matrix" %in% names(result$structs))
  matrix_fields <- result$structs$Matrix
  field_names <- sapply(matrix_fields, function(f) f$name)

  expect_true("data" %in% field_names)
  data_field <- matrix_fields[[which(field_names == "data")]]
  expect_match(data_field$type, "int\\[3\\]\\[4\\]")
})
