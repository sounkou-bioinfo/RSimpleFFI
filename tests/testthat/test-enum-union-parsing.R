test_that("ffi_parse_header extracts simple enum", {
  skip_if_not(tcc_available(), "TinyCC not available")
  skip_if_not_installed("treesitter")
  skip_if_not_installed("treesitter.c")

  header <- system.file("extdata", "enums_unions.h", package = "RSimpleFFI")
  parsed <- ffi_parse_header(header)
  enums <- parsed$enums

  expect_true("Color" %in% names(enums))
  expect_equal(enums$Color["RED"], c(RED = 0L))
  expect_equal(enums$Color["GREEN"], c(GREEN = 1L))
  expect_equal(enums$Color["BLUE"], c(BLUE = 2L))
})

test_that("ffi_parse_header extracts typedef enum", {
  skip_if_not(tcc_available(), "TinyCC not available")
  skip_if_not_installed("treesitter")
  skip_if_not_installed("treesitter.c")

  header <- system.file("extdata", "enums_unions.h", package = "RSimpleFFI")
  parsed <- ffi_parse_header(header)
  enums <- parsed$enums

  expect_true("Status" %in% names(enums))
  expect_equal(enums$Status["STATUS_OK"], c(STATUS_OK = 0L))
  expect_equal(enums$Status["STATUS_ERROR"], c(STATUS_ERROR = 1L))
  expect_equal(enums$Status["STATUS_PENDING"], c(STATUS_PENDING = 2L))
  expect_equal(enums$Status["STATUS_CANCELLED"], c(STATUS_CANCELLED = 3L))
})

test_that("ffi_parse_header handles explicit enum values", {
  skip_if_not(tcc_available(), "TinyCC not available")
  skip_if_not_installed("treesitter")
  skip_if_not_installed("treesitter.c")

  header <- system.file("extdata", "enums_unions.h", package = "RSimpleFFI")
  parsed <- ffi_parse_header(header)
  enums <- parsed$enums

  expect_true("Flags" %in% names(enums))
  expect_equal(enums$Flags["FLAG_NONE"], c(FLAG_NONE = 0L))
  expect_equal(enums$Flags["FLAG_READ"], c(FLAG_READ = 1L))
  expect_equal(enums$Flags["FLAG_WRITE"], c(FLAG_WRITE = 2L))
  expect_equal(enums$Flags["FLAG_EXECUTE"], c(FLAG_EXECUTE = 4L))
  expect_equal(enums$Flags["FLAG_ALL"], c(FLAG_ALL = 7L))
})

test_that("ffi_parse_header handles gaps in enum values", {
  skip_if_not(tcc_available(), "TinyCC not available")
  skip_if_not_installed("treesitter")
  skip_if_not_installed("treesitter.c")

  header <- system.file("extdata", "enums_unions.h", package = "RSimpleFFI")
  parsed <- ffi_parse_header(header)
  enums <- parsed$enums

  expect_true("Priority" %in% names(enums))
  expect_equal(enums$Priority["PRIORITY_LOW"], c(PRIORITY_LOW = 0L))
  expect_equal(enums$Priority["PRIORITY_MEDIUM"], c(PRIORITY_MEDIUM = 5L))
  expect_equal(enums$Priority["PRIORITY_HIGH"], c(PRIORITY_HIGH = 10L))
  expect_equal(enums$Priority["PRIORITY_CRITICAL"], c(PRIORITY_CRITICAL = 100L))
})

test_that("tinycc extracts unions correctly", {
  skip_if_not(tcc_available(), "TinyCC not available")

  header_file <- system.file(
    "extdata",
    "enums_unions.h",
    package = "RSimpleFFI"
  )
  parsed <- ffi_parse_header(header_file)

  # Check unions were extracted
  expect_true("unions" %in% names(parsed))
  expect_true(length(parsed$unions) > 0)

  # Check specific unions
  expect_true("Value" %in% names(parsed$unions))
  expect_true("Data" %in% names(parsed$unions))
  expect_true("IntUnion" %in% names(parsed$unions))

  # Check Value union fields
  value_union <- parsed$unions$Value
  expect_equal(length(value_union), 3)
  field_names <- sapply(value_union, function(f) f$name)
  expect_true("as_int" %in% field_names)
  expect_true("as_float" %in% field_names)
  expect_true("as_double" %in% field_names)

  # Check IntUnion fields
  int_union <- parsed$unions$IntUnion
  expect_equal(length(int_union), 4)
  field_names <- sapply(int_union, function(f) f$name)
  expect_true("byte" %in% field_names)
  expect_true("word" %in% field_names)
  expect_true("dword" %in% field_names)
  expect_true("qword" %in% field_names)
})

test_that("ffi_parse_header extracts typedef union", {
  skip_if_not(tcc_available(), "TinyCC not available")
  skip_if_not_installed("treesitter")
  skip_if_not_installed("treesitter.c")

  header <- system.file("extdata", "enums_unions.h", package = "RSimpleFFI")
  parsed <- ffi_parse_header(header)
  unions <- parsed$unions

  expect_true("Data" %in% names(unions))
  expect_equal(length(unions$Data), 3)
  expect_equal(unions$Data[[1]]$name, "i")
  expect_equal(unions$Data[[1]]$type, "int")
  expect_equal(unions$Data[[2]]$name, "f")
  expect_equal(unions$Data[[2]]$type, "float")
})

test_that("ffi_parse_header handles complex union types", {
  skip_if_not(tcc_available(), "TinyCC not available")
  skip_if_not_installed("treesitter")
  skip_if_not_installed("treesitter.c")

  header <- system.file("extdata", "enums_unions.h", package = "RSimpleFFI")
  parsed <- ffi_parse_header(header)
  unions <- parsed$unions

  expect_true("IntUnion" %in% names(unions))
  expect_equal(length(unions$IntUnion), 4)
  expect_equal(unions$IntUnion[[1]]$type, "uint8_t")
  expect_equal(unions$IntUnion[[2]]$type, "uint16_t")
  expect_equal(unions$IntUnion[[3]]$type, "uint32_t")
  expect_equal(unions$IntUnion[[4]]$type, "uint64_t")
})

test_that("ffi_parse_header includes enums and unions", {
  skip_if_not(tcc_available(), "TinyCC not available")

  header <- system.file("extdata", "enums_unions.h", package = "RSimpleFFI")
  parsed <- ffi_parse_header(header)

  expect_true("enums" %in% names(parsed))
  expect_true("unions" %in% names(parsed))
  expect_true("structs" %in% names(parsed))

  expect_true("Color" %in% names(parsed$enums))
  expect_true("Status" %in% names(parsed$enums))
  expect_true("Value" %in% names(parsed$unions))
  expect_true("Data" %in% names(parsed$unions))
})

test_that("generate_enum_definition creates valid R code", {
  Color <- c(RED = 0L, GREEN = 1L, BLUE = 2L)
  code <- generate_enum_definition("Color", Color)

  expect_true(grepl("Color <- ffi_enum\\(", code))
  expect_true(grepl("RED = 0L", code))
  expect_true(grepl("GREEN = 1L", code))
  expect_true(grepl("BLUE = 2L", code))

  # Should have commas between values
  expect_true(grepl("RED = 0L,", code))
  expect_true(grepl("GREEN = 1L,", code))

  # Verify generated code is valid R
  env <- new.env()
  eval(parse(text = code), envir = env)
  expect_true(exists("Color", envir = env))
  expect_true(S7::S7_inherits(env$Color, EnumType))
})

test_that("generate_union_definition creates valid R code", {
  Value <- list(
    list(name = "as_int", type = "int"),
    list(name = "as_float", type = "float"),
    list(name = "as_double", type = "double")
  )
  code <- generate_union_definition("Value", Value)

  expect_true(grepl("Value <- ffi_union\\(", code))
  expect_true(grepl("as_int = ffi_int\\(\\)", code))
  expect_true(grepl("as_float = ffi_float\\(\\)", code))
  expect_true(grepl("as_double = ffi_double\\(\\)", code))

  # Verify generated code is valid R
  env <- new.env()
  eval(parse(text = code), envir = env)
  expect_true(exists("Value", envir = env))
  expect_true(S7::S7_inherits(env$Value, UnionType))
})

test_that("generate_r_bindings includes enums and unions", {
  skip_if_not(tcc_available(), "TinyCC not available")

  header <- system.file("extdata", "enums_unions.h", package = "RSimpleFFI")
  parsed <- ffi_parse_header(header)

  tmp_file <- tempfile(fileext = ".R")
  on.exit(unlink(tmp_file))

  bindings <- generate_r_bindings(parsed, output_file = tmp_file)

  expect_true(file.exists(tmp_file))
  code <- readLines(tmp_file)
  code_text <- paste(code, collapse = "\n")

  # Check for enum definitions
  expect_true(grepl("# Enum definitions", code_text))
  expect_true(grepl("Color <- ffi_enum\\(", code_text))
  expect_true(grepl("Status <- ffi_enum\\(", code_text))

  # Check for union definitions
  expect_true(grepl("# Union definitions", code_text))
  expect_true(grepl("Value <- ffi_union\\(", code_text))
  expect_true(grepl("Data <- ffi_union\\(", code_text))

  # Verify the code is valid R and creates working types
  env <- new.env()
  source(tmp_file, local = env)

  expect_true(exists("Color", envir = env))
  expect_true(exists("Status", envir = env))
  expect_true(exists("Value", envir = env))
  expect_true(exists("Data", envir = env))

  expect_true(S7::S7_inherits(env$Color, EnumType))
  expect_true(S7::S7_inherits(env$Value, UnionType))
})

test_that("mixed types header is parsed correctly", {
  skip_if_not(tcc_available(), "TinyCC not available")

  header <- system.file("extdata", "mixed_types.h", package = "RSimpleFFI")
  parsed <- ffi_parse_header(header)

  # Check enums
  expect_true("MachineState" %in% names(parsed$enums))
  expect_true("MachineEvent" %in% names(parsed$enums))
  expect_true("Protocol" %in% names(parsed$enums))

  # Check unions (EventData has nested struct, may not parse)
  expect_true("Address" %in% names(parsed$unions))

  # Check structs that contain enums/unions
  expect_true("Event" %in% names(parsed$structs))
  expect_true("StateMachine" %in% names(parsed$structs))
  expect_true("Packet" %in% names(parsed$structs))
})

test_that("generated code from mixed types is valid", {
  skip_if_not(tcc_available(), "TinyCC not available")

  header <- system.file("extdata", "mixed_types.h", package = "RSimpleFFI")
  parsed <- ffi_parse_header(header)

  tmp_file <- tempfile(fileext = ".R")
  on.exit(unlink(tmp_file))

  bindings <- generate_r_bindings(parsed, output_file = tmp_file)

  # Source and verify all types are created
  env <- new.env()
  source(tmp_file, local = env)

  # Enums should exist and be EnumType
  expect_true(exists("MachineState", envir = env))
  expect_true(S7::S7_inherits(env$MachineState, EnumType))
  expect_equal(env$MachineState@values["STATE_IDLE"], c(STATE_IDLE = 0L))

  # Note: EventData union with anonymous nested struct is not currently supported
  # by our simple regex parser, but simpler unions like Address work fine
  expect_true(exists("Address", envir = env))
  expect_true(S7::S7_inherits(env$Address, UnionType))

  # Structs should exist and be StructType
  expect_true(exists("Event", envir = env))
  expect_true(S7::S7_inherits(env$Event, StructType))
})

test_that("enum with reserved R names are escaped", {
  skip_if_not(tcc_available(), "TinyCC not available")

  # Create temporary header with reserved words
  tmp_header <- tempfile(fileext = ".h")
  on.exit(unlink(tmp_header))

  writeLines(
    c(
      "enum Keywords {",
      "    if = 0,",
      "    else = 1,",
      "    for = 2",
      "};"
    ),
    tmp_header
  )

  parsed <- ffi_parse_header(tmp_header)
  code <- generate_enum_definition("Keywords", parsed$enums$Keywords)

  # Reserved words should be escaped with backticks
  expect_true(grepl("`if` = 0L", code, fixed = TRUE))
  expect_true(grepl("`else` = 1L", code, fixed = TRUE))
  expect_true(grepl("`for` = 2L", code, fixed = TRUE))

  # Verify code is valid
  env <- new.env()
  eval(parse(text = code), envir = env)
  expect_true(exists("Keywords", envir = env))
})

test_that("union with reserved R names are escaped", {
  skip_if_not(tcc_available(), "TinyCC not available")

  # Create temporary header with reserved words
  tmp_header <- tempfile(fileext = ".h")
  on.exit(unlink(tmp_header))

  writeLines(
    c(
      "union Reserved {",
      "    int if;",
      "    float while;",
      "    double for;",
      "};"
    ),
    tmp_header
  )

  parsed <- ffi_parse_header(tmp_header)
  code <- generate_union_definition("Reserved", parsed$unions$Reserved)

  # Reserved words should be escaped with backticks
  expect_true(grepl("`if` = ffi_int\\(\\)", code))
  expect_true(grepl("`while` = ffi_float\\(\\)", code))
  expect_true(grepl("`for` = ffi_double\\(\\)", code))

  # Verify code is valid
  env <- new.env()
  eval(parse(text = code), envir = env)
  expect_true(exists("Reserved", envir = env))
})
