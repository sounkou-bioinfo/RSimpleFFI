test_that("parser detects bit-fields in structs", {
  skip_if_not(tcc_available(), "TinyCC not available")

  # Create a header with bit-fields
  header_file <- tempfile(fileext = ".h")
  writeLines(
    c(
      "typedef struct {",
      "  unsigned int enabled : 1;",
      "  unsigned int mode : 3;",
      "  unsigned int reserved : 4;",
      "} Flags;"
    ),
    header_file
  )

  # Parse header
  parsed <- ffi_parse_header(header_file)

  # Check that struct was parsed
  expect_true("Flags" %in% names(parsed$structs))

  # Check that bit-field warning attribute is attached
  flags_struct <- parsed$structs$Flags
  bitfield_warning <- attr(flags_struct, "bitfield_warning")

  expect_true(!is.null(bitfield_warning))
  expect_true(bitfield_warning$has_bitfields)
  expect_length(bitfield_warning$fields, 3)
  expect_true("'enabled : 1'" %in% bitfield_warning$fields)
  expect_true("'mode : 3'" %in% bitfield_warning$fields)
  expect_true("'reserved : 4'" %in% bitfield_warning$fields)

  unlink(header_file)
})

test_that("parser distinguishes bit-fields from arrays", {
  skip_if_not(tcc_available(), "TinyCC not available")

  # Arrays use brackets, bit-fields use colons
  header_file <- tempfile(fileext = ".h")
  writeLines(
    c(
      "typedef struct {",
      "  int array[3];", # array - not a bit-field
      "  unsigned int flags : 3;", # bit-field
      "  char name[10];", # array - not a bit-field
      "} Mixed;"
    ),
    header_file
  )

  parsed <- ffi_parse_header(header_file)
  mixed_struct <- parsed$structs$Mixed
  bitfield_warning <- attr(mixed_struct, "bitfield_warning")

  # Should only detect the bit-field, not arrays
  expect_true(!is.null(bitfield_warning))
  expect_true(bitfield_warning$has_bitfields)
  expect_length(bitfield_warning$fields, 1)
  expect_true("'flags : 3'" %in% bitfield_warning$fields)

  unlink(header_file)
})

test_that("parser handles structs without bit-fields", {
  skip_if_not(tcc_available(), "TinyCC not available")

  header_file <- tempfile(fileext = ".h")
  writeLines(
    c(
      "typedef struct {",
      "  int x;",
      "  int y;",
      "  double z;",
      "} Point;"
    ),
    header_file
  )

  parsed <- ffi_parse_header(header_file)
  point_struct <- parsed$structs$Point
  bitfield_warning <- attr(point_struct, "bitfield_warning")

  # Should have no bit-field warning
  expect_true(is.null(bitfield_warning))

  unlink(header_file)
})

test_that("generate_struct_definition warns about bit-fields", {
  skip_if_not(tcc_available(), "TinyCC not available")

  header_file <- tempfile(fileext = ".h")
  writeLines(
    c(
      "typedef struct {",
      "  unsigned int enabled : 1;",
      "  unsigned int mode : 3;",
      "} Settings;"
    ),
    header_file
  )

  parsed <- ffi_parse_header(header_file)

  # Should generate warning when creating struct definition
  expect_warning(
    code <- generate_struct_definition("Settings", parsed$structs$Settings),
    "contains bit-fields which are not supported"
  )

  expect_warning(
    code <- generate_struct_definition("Settings", parsed$structs$Settings),
    "README section 'Bit-fields and Struct Packing'"
  )

  # Should still generate code (even if not usable as-is)
  expect_type(code, "character")

  unlink(header_file)
})

test_that("parser detects various bit-field widths", {
  skip_if_not(tcc_available(), "TinyCC not available")

  header_file <- tempfile(fileext = ".h")
  writeLines(
    c(
      "typedef struct {",
      "  unsigned int a : 1;",
      "  unsigned int b : 2;",
      "  unsigned int c : 5;",
      "  unsigned int d : 8;",
      "  unsigned int e : 16;",
      "} VariousWidths;"
    ),
    header_file
  )

  parsed <- ffi_parse_header(header_file)
  struct_def <- parsed$structs$VariousWidths
  bitfield_warning <- attr(struct_def, "bitfield_warning")

  expect_true(!is.null(bitfield_warning))
  expect_length(bitfield_warning$fields, 5)

  # Check specific widths are captured
  expect_true("'a : 1'" %in% bitfield_warning$fields)
  expect_true("'b : 2'" %in% bitfield_warning$fields)
  expect_true("'c : 5'" %in% bitfield_warning$fields)
  expect_true("'d : 8'" %in% bitfield_warning$fields)
  expect_true("'e : 16'" %in% bitfield_warning$fields)

  unlink(header_file)
})

test_that("parser handles mixed regular fields and bit-fields", {
  skip_if_not(tcc_available(), "TinyCC not available")

  header_file <- tempfile(fileext = ".h")
  writeLines(
    c(
      "typedef struct {",
      "  int id;", # regular field
      "  unsigned int flags : 8;", # bit-field
      "  char* name;", # regular field
      "  unsigned int mode : 3;", # bit-field
      "  double value;", # regular field
      "} MixedStruct;"
    ),
    header_file
  )

  parsed <- ffi_parse_header(header_file)
  struct_def <- parsed$structs$MixedStruct

  # Should have 5 fields total
  expect_length(struct_def, 5)

  # Should detect 2 bit-fields
  bitfield_warning <- attr(struct_def, "bitfield_warning")
  expect_true(!is.null(bitfield_warning))
  expect_length(bitfield_warning$fields, 2)
  expect_true("'flags : 8'" %in% bitfield_warning$fields)
  expect_true("'mode : 3'" %in% bitfield_warning$fields)

  unlink(header_file)
})

test_that("full workflow with bit-fields generates warning", {
  skip_if_not(tcc_available(), "TinyCC not available")

  header_file <- tempfile(fileext = ".h")
  writeLines(
    c(
      "typedef struct {",
      "  unsigned int enabled : 1;",
      "  unsigned int priority : 4;",
      "} Config;",
      "",
      "void set_config(unsigned int config);"
    ),
    header_file
  )

  # Parse and generate bindings should warn
  parsed <- ffi_parse_header(header_file)

  expect_warning(
    bindings <- generate_r_bindings(parsed),
    "Config.*contains bit-fields"
  )

  # Bindings should still be generated
  expect_type(bindings, "character")
  expect_true(grepl("set_config", bindings))

  unlink(header_file)
})
