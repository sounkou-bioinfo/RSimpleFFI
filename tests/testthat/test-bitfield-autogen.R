test_that("bit-field detection in parsed headers", {
  header <- tempfile(fileext = ".h")
  writeLines(c(
    "typedef struct {",
    "  unsigned int enabled : 1;",
    "  unsigned int mode : 3;",
    "  int priority : 4;",
    "} Config;"
  ), header)
  on.exit(unlink(header), add = TRUE)

  parsed <- ffi_parse_header(header)

  # Check struct was parsed
  expect_true("Config" %in% names(parsed$structs))

  # Check bit-field attribute is attached to the struct itself
  struct_def <- parsed$structs$Config
  bf_warning <- attr(struct_def, "bitfield_warning")

  expect_true(!is.null(bf_warning))
  expect_true(bf_warning$has_bitfields)
  expect_true("'enabled : 1'" %in% bf_warning$fields)
  expect_true("'mode : 3'" %in% bf_warning$fields)
  expect_true("'priority : 4'" %in% bf_warning$fields)
})

test_that("full header parse generates accessor code", {
  header <- tempfile(fileext = ".h")
  writeLines(c(
    "typedef struct {",
    "  unsigned int enabled : 1;",
    "  unsigned int mode : 3;",
    "  unsigned int priority : 4;",
    "} Config;"
  ), header)
  on.exit(unlink(header), add = TRUE)

  parsed <- ffi_parse_header(header)

  # Warning comes from generate_r_bindings, not ffi_parse_header
  expect_warning(
    code <- generate_r_bindings(parsed),
    "bit-fields"
  )

  expect_true(grepl("Config <- ffi_create_bitfield_accessors", code, fixed = TRUE))
  expect_true(grepl("enabled = 1L", code, fixed = TRUE))
  expect_true(grepl("mode = 3L", code, fixed = TRUE))
  expect_true(grepl("priority = 4L", code, fixed = TRUE))
  expect_true(grepl("pack", code, fixed = TRUE))
  expect_true(grepl("get", code, fixed = TRUE))
  expect_true(grepl("set", code, fixed = TRUE))
})

test_that("generated accessor code is valid R and works", {
  header <- tempfile(fileext = ".h")
  writeLines(c(
    "typedef struct {",
    "  unsigned int flag : 1;",
    "  unsigned int level : 4;",
    "} Status;"
  ), header)
  on.exit(unlink(header), add = TRUE)

  parsed <- ffi_parse_header(header)
  expect_warning(
    code <- generate_r_bindings(parsed),
    "bit-fields"
  )

  # Extract just the accessor definition
  accessor_start <- regexpr("Status <- ffi_create_bitfield_accessors", code)
  accessor_end <- regexpr("# Usage:", code)
  accessor_code <- substr(code, accessor_start, accessor_end - 1)

  # Evaluate the accessor code
  eval(parse(text = accessor_code))

  # Test the generated accessor
  expect_true(exists("Status"))
  expect_true(is.list(Status))
  expect_true(all(c("pack", "unpack", "get", "set") %in% names(Status)))

  # Test pack/unpack
  packed <- Status$pack(list(flag = 1L, level = 5L))
  expect_type(packed, "integer")

  unpacked <- Status$unpack(packed)
  expect_equal(unpacked$flag, 1L)
  expect_equal(unpacked$level, 5L)

  # Test get/set
  expect_equal(Status$get(packed, "flag"), 1L)
  expect_equal(Status$get(packed, "level"), 5L)

  packed2 <- Status$set(packed, "level", 10L)
  expect_equal(Status$get(packed2, "level"), 10L)
})

test_that("auto-generated accessor works with C test functions", {
  # Generate accessor for SettingsFlags from test_functions.c
  header <- tempfile(fileext = ".h")
  writeLines(c(
    "typedef struct {",
    "  unsigned int enabled : 1;",
    "  unsigned int mode : 3;",
    "  unsigned int priority : 4;",
    "  unsigned int reserved : 24;",
    "} SettingsFlags;",
    "",
    "int test_bitfield_get_enabled(uint32_t packed);",
    "int test_bitfield_get_mode(uint32_t packed);",
    "int test_bitfield_get_priority(uint32_t packed);",
    "uint32_t test_bitfield_pack(int enabled, int mode, int priority);",
    "int test_bitfield_verify(uint32_t packed, int e, int m, int p);",
    "uint32_t test_bitfield_increment_priority(uint32_t packed);"
  ), header)
  on.exit(unlink(header), add = TRUE)

  parsed <- ffi_parse_header(header)
  expect_warning(
    code <- generate_r_bindings(parsed),
    "bit-fields"
  )

  # Extract and evaluate accessor
  accessor_start <- regexpr("SettingsFlags <- ffi_create_bitfield_accessors", code)
  accessor_end <- regexpr("# Usage:", code)
  accessor_code <- substr(code, accessor_start, accessor_end - 1)
  eval(parse(text = accessor_code))

  # Pack values in R
  packed <- SettingsFlags$pack(list(enabled = 1L, mode = 5L, priority = 10L))

  # Verify with C
  get_enabled <- ffi_function("test_bitfield_get_enabled", ffi_int(), ffi_uint32())
  get_mode <- ffi_function("test_bitfield_get_mode", ffi_int(), ffi_uint32())
  get_priority <- ffi_function("test_bitfield_get_priority", ffi_int(), ffi_uint32())

  expect_equal(get_enabled(packed), 1L)
  expect_equal(get_mode(packed), 5L)
  expect_equal(get_priority(packed), 10L)

  # Verify with C verification function
  verify <- ffi_function(
    "test_bitfield_verify", ffi_int(),
    ffi_uint32(), ffi_int(), ffi_int(), ffi_int()
  )
  expect_equal(verify(packed, 1L, 5L, 10L), 1L)

  # Modify in C, read in R
  increment <- ffi_function("test_bitfield_increment_priority", ffi_uint32(), ffi_uint32())
  packed2 <- increment(packed)

  expect_equal(SettingsFlags$get(packed2, "priority"), 11L)
  expect_equal(get_priority(packed2), 11L)
})

test_that("auto-generated accessor handles PacketFlags from test_functions.c", {
  # Generate accessor for 8-bit PacketFlags
  header <- tempfile(fileext = ".h")
  writeLines(c(
    "typedef struct {",
    "  unsigned int syn : 1;",
    "  unsigned int ack : 1;",
    "  unsigned int fin : 1;",
    "  unsigned int rst : 1;",
    "  unsigned int reserved : 4;",
    "} PacketFlags;",
    "",
    "int test_packet_has_ack(uint8_t packed);",
    "uint8_t test_packet_create_synack(void);",
    "int test_packet_count_flags(uint8_t packed);"
  ), header)
  on.exit(unlink(header), add = TRUE)

  parsed <- ffi_parse_header(header)
  expect_warning(
    code <- generate_r_bindings(parsed),
    "bit-fields"
  )

  # Extract and evaluate accessor
  accessor_start <- regexpr("PacketFlags <- ffi_create_bitfield_accessors", code)
  accessor_end <- regexpr("# Usage:", code)
  accessor_code <- substr(code, accessor_start, accessor_end - 1)
  eval(parse(text = accessor_code))

  # Create SYN+ACK in C
  create_synack <- ffi_function("test_packet_create_synack", ffi_uint8())
  packed <- create_synack()

  # Read flags in R using auto-generated accessor
  expect_equal(PacketFlags$get(packed, "syn"), 1L)
  expect_equal(PacketFlags$get(packed, "ack"), 1L)
  expect_equal(PacketFlags$get(packed, "fin"), 0L)
  expect_equal(PacketFlags$get(packed, "rst"), 0L)

  # Verify with C
  has_ack <- ffi_function("test_packet_has_ack", ffi_int(), ffi_uint8())
  count_flags <- ffi_function("test_packet_count_flags", ffi_int(), ffi_uint8())

  expect_equal(has_ack(packed), 1L)
  expect_equal(count_flags(packed), 2L)

  # Create FIN packet in R
  fin_packet <- PacketFlags$pack(list(syn = 0L, ack = 0L, fin = 1L, rst = 0L))
  expect_equal(count_flags(fin_packet), 1L)
  expect_equal(has_ack(fin_packet), 0L)
})

test_that("no accessor generated for structs without bit-fields", {
  header <- tempfile(fileext = ".h")
  writeLines(c(
    "typedef struct {",
    "  int x;",
    "  int y;",
    "} Point;"
  ), header)
  on.exit(unlink(header), add = TRUE)

  parsed <- ffi_parse_header(header)
  code <- generate_r_bindings(parsed)

  expect_false(grepl("ffi_create_bitfield_accessors", code, fixed = TRUE))
  expect_true(grepl("Point <- ffi_struct", code, fixed = TRUE))
})

test_that("mixed struct with bit-fields and normal fields generates accessor only", {
  # Note: libffi can't handle mixed structs, so we only generate accessor
  header <- tempfile(fileext = ".h")
  writeLines(c(
    "typedef struct {",
    "  int id;",
    "  unsigned int flag : 1;",
    "  unsigned int level : 4;",
    "  double value;",
    "} MixedStruct;"
  ), header)
  on.exit(unlink(header), add = TRUE)

  parsed <- ffi_parse_header(header)

  # Warning comes from generate_r_bindings
  expect_warning(
    code <- generate_r_bindings(parsed),
    "bit-fields"
  )

  # Should generate accessor for bit-fields
  expect_true(grepl("MixedStruct <- ffi_create_bitfield_accessors", code, fixed = TRUE))
  expect_true(grepl("flag = 1L", code, fixed = TRUE))
  expect_true(grepl("level = 4L", code, fixed = TRUE))
})
