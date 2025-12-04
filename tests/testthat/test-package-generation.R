# Tests for Package Generation Functions

library(testthat)
library(RSimpleFFI)

test_that("generate_package_init creates valid R code", {
  init_code <- generate_package_init(
    "mylib",
    "MyPackage",
    use_system_lib = TRUE
  )

  expect_type(init_code, "character")
  expect_true(nchar(init_code) > 0)
  expect_true(grepl(".onLoad", init_code, fixed = TRUE))
  expect_true(grepl(".onUnload", init_code, fixed = TRUE))
  expect_true(grepl("dll_load_system", init_code))
  expect_true(grepl("mylib", init_code))
})

test_that("generate_package_init handles bundled libraries", {
  init_code <- generate_package_init(
    "mylib",
    "MyPackage",
    use_system_lib = FALSE
  )

  expect_true(grepl("system.file", init_code))
  expect_false(grepl("dll_load_system", init_code))
})

test_that("generate_package_init handles custom library path", {
  init_code <- generate_package_init(
    "mylib",
    "MyPackage",
    use_system_lib = FALSE,
    library_path = "/custom/path/libmylib.so"
  )

  expect_true(grepl("/custom/path/libmylib.so", init_code, fixed = TRUE))
  expect_true(grepl("dll_load", init_code))
})

test_that("generate_package_from_headers creates complete package structure", {
  skip_if_not(tcc_available(), "TCC not available")

  header <- system.file("extdata", "simple_types.h", package = "RSimpleFFI")
  skip_if(header == "", "Test header not found")

  tmpdir <- tempfile()
  dir.create(tmpdir)

  result <- generate_package_from_headers(
    header_files = header,
    package_name = "TestPkg",
    library_name = "testlib",
    output_dir = tmpdir,
    use_system_lib = TRUE
  )

  # Check package structure
  expect_true(file.exists(file.path(tmpdir, "DESCRIPTION")))
  expect_true(file.exists(file.path(tmpdir, "NAMESPACE")))
  expect_true(dir.exists(file.path(tmpdir, "R")))
  expect_true(file.exists(file.path(tmpdir, "R", "zzz.R")))

  # Check for generated binding files in R/ subfolder
  r_files <- list.files(file.path(tmpdir, "R"), pattern = "_bindings\\.R$")
  expect_true(length(r_files) > 0)

  # Check bindings file content
  bindings <- readLines(file.path(tmpdir, "R", r_files[1]))
  expect_true(any(grepl("r_add", bindings)))
  expect_true(any(grepl("Point", bindings)))

  # Check zzz file
  zzz <- readLines(file.path(tmpdir, "R", "zzz.R"))
  expect_true(any(grepl(".onLoad", zzz)))
  expect_true(any(grepl("dll_load_system", zzz)))

  # Check DESCRIPTION file
  desc <- readLines(file.path(tmpdir, "DESCRIPTION"))
  expect_true(any(grepl("Package: TestPkg", desc)))
  expect_true(any(grepl("Imports:", desc)))
  expect_true(any(grepl("RSimpleFFI", desc)))

  # Check NAMESPACE file
  ns <- readLines(file.path(tmpdir, "NAMESPACE"))
  expect_true(any(grepl("import\\(RSimpleFFI\\)", ns)))
  expect_true(any(grepl("exportPattern", ns)))

  unlink(tmpdir, recursive = TRUE)
})

test_that("escape_r_name escapes invalid names", {
  # Valid names - no escaping
  expect_equal(escape_r_name("x"), "x")
  expect_equal(escape_r_name("Point"), "Point")
  expect_equal(escape_r_name("my_var"), "my_var")

  # Invalid names - need backticks
  expect_equal(escape_r_name("__foo"), "`__foo`")
  expect_equal(escape_r_name("_bar"), "`_bar`")
  expect_equal(escape_r_name("123"), "`123`")
})
