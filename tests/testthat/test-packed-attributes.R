test_that("tree-sitter detects packed struct attributes", {
  skip_if_not_installed("treesitter")
  skip_if_not_installed("treesitter.c")

  header <- system.file("headers/test_packed.h", package = "RSimpleFFI")
  if (!file.exists(header)) {
    skip("Test header not found")
  }

  result <- ffi_parse_header(header)

  # Check that PackedStruct has packed attribute
  expect_true("PackedStruct" %in% names(result$structs))
  expect_true(isTRUE(attr(result$structs$PackedStruct, "packed")))

  # Check that PackedTypedef has packed attribute
  expect_true("PackedTypedef" %in% names(result$structs))
  expect_true(isTRUE(attr(result$structs$PackedTypedef, "packed")))

  # Check that NormalStruct does NOT have packed attribute
  expect_true("NormalStruct" %in% names(result$structs))
  expect_false(isTRUE(attr(result$structs$NormalStruct, "packed")))
})

test_that("packed attribute is preserved in generated code", {
  skip_if_not_installed("treesitter")
  skip_if_not_installed("treesitter.c")

  header <- system.file("headers/test_packed.h", package = "RSimpleFFI")
  if (!file.exists(header)) {
    skip("Test header not found")
  }

  result <- ffi_parse_header_ts(header)

  # Generate code for each struct (packed structs will generate warnings)
  expect_warning(
    packed_struct_code <- generate_struct_definition(
      "PackedStruct",
      result$structs$PackedStruct
    ),
    "packed"
  )
  expect_warning(
    packed_typedef_code <- generate_struct_definition(
      "PackedTypedef",
      result$structs$PackedTypedef
    ),
    "packed"
  )
  normal_struct_code <- generate_struct_definition(
    "NormalStruct",
    result$structs$NormalStruct
  )

  # Check that generated code includes pack=1 for packed structs
  expect_match(packed_struct_code, "pack\\s*=\\s*1", perl = TRUE)
  expect_match(packed_typedef_code, "pack\\s*=\\s*1", perl = TRUE)

  # Check that normal struct doesn't have pack parameter
  # (or if it does, it's not pack=1)
  expect_false(grepl("pack\\s*=\\s*1", normal_struct_code, perl = TRUE))
})
