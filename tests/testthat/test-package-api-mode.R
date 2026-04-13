test_that("API mode package generation creates proper structure", {
  skip_on_cran()
  skip_if_not_installed("RSimpleFFI")
  
  # Create test header
  header_file <- tempfile(fileext = ".h")
  writeLines(c(
    "#ifndef TEST_API_H",
    "#define TEST_API_H",
    "",
    "struct Point {",
    "    int x;",
    "    int y;",
    "};",
    "",
    "struct Rectangle {",
    "    int width;",
    "    int height;",
    "    struct Point origin;",
    "};",
    "",
    "int calculate_area(struct Rectangle* rect);",
    "",
    "#endif"
  ), header_file)
  
  # Generate package with API mode
  pkg_dir <- tempfile()
  result <- generate_package_from_headers(
    header_files = header_file,
    package_name = "TestAPIPkg",
    library_name = "testlib",
    output_dir = pkg_dir,
    use_api_mode = TRUE,
    use_system_lib = TRUE,
    authors_r = 'person("Test", "Author", email = "test@example.com", role = c("aut", "cre"))'
  )
  
  # Check package structure
  expect_true(dir.exists(pkg_dir))
  expect_true(dir.exists(file.path(pkg_dir, "R")))
  expect_true(dir.exists(file.path(pkg_dir, "src")))
  
  # Check required files exist
  expect_true(file.exists(file.path(pkg_dir, "DESCRIPTION")))
  expect_true(file.exists(file.path(pkg_dir, "NAMESPACE")))
  expect_true(file.exists(file.path(pkg_dir, "LICENSE")))
  
  # Check src/ files
  expect_true(file.exists(file.path(pkg_dir, "src", "init.c")))
  expect_true(file.exists(file.path(pkg_dir, "src", "struct_helpers.c")))
  expect_true(file.exists(file.path(pkg_dir, "src", "Makevars")))
  expect_true(file.exists(file.path(pkg_dir, "src", "Makevars.win")))
  
  # Check R files for struct wrappers
  expect_true(file.exists(file.path(pkg_dir, "R", "Point_api.R")))
  expect_true(file.exists(file.path(pkg_dir, "R", "Rectangle_api.R")))
  
  # Verify init.c content
  init_c <- readLines(file.path(pkg_dir, "src", "init.c"))
  expect_true(any(grepl("R_init_TestAPIPkg", init_c)))
  expect_true(any(grepl("rffi_Point_new", init_c)))
  expect_true(any(grepl("rffi_Point_offsets", init_c)))
  expect_true(any(grepl("rffi_Rectangle_new", init_c)))
  expect_true(any(grepl("rffi_Rectangle_offsets", init_c)))
  
  # Verify struct_helpers.c content
  helpers_c <- readLines(file.path(pkg_dir, "src", "struct_helpers.c"))
  expect_true(any(grepl("typedef struct", helpers_c)))
  expect_true(any(grepl("offsetof", helpers_c)))
  expect_true(any(grepl("SEXP rffi_Point_new", helpers_c)))
  expect_true(any(grepl("SEXP rffi_Rectangle_offsets", helpers_c)))
  
  # Verify R wrappers
  point_api <- readLines(file.path(pkg_dir, "R", "Point_api.R"))
  expect_true(any(grepl("Point_new <- function", point_api)))
  expect_true(any(grepl("Point_get <- function", point_api)))
  expect_true(any(grepl("Point_set <- function", point_api)))
  expect_true(any(grepl('\\.Call\\(rffi_Point_new', point_api)))
  
  # Clean up
  unlink(header_file)
  unlink(pkg_dir, recursive = TRUE)
})

test_that("API mode handles structs with bitfields", {
  skip_on_cran()
  skip_if_not_installed("RSimpleFFI")
  
  # Create header with bitfield struct
  header_file <- tempfile(fileext = ".h")
  writeLines(c(
    "struct Flags {",
    "    unsigned int enabled : 1;",
    "    unsigned int mode : 3;",
    "    unsigned int priority : 4;",
    "    unsigned int reserved : 24;",
    "};",
    "",
    "struct Config {",
    "    int id;",
    "    struct Flags flags;",
    "    char* name;",
    "};"
  ), header_file)
  
  # Generate package
  pkg_dir <- tempfile()
  result <- generate_package_from_headers(
    header_files = header_file,
    package_name = "TestBitfieldPkg",
    library_name = "testlib",
    output_dir = pkg_dir,
    use_api_mode = TRUE,
    use_system_lib = FALSE
  )
  
  # Verify files were generated
  expect_true(file.exists(file.path(pkg_dir, "src", "struct_helpers.c")))
  expect_true(file.exists(file.path(pkg_dir, "R", "Flags_api.R")))
  expect_true(file.exists(file.path(pkg_dir, "R", "Config_api.R")))
  
  # Verify struct_helpers.c handles bitfields
  helpers_c <- readLines(file.path(pkg_dir, "src", "struct_helpers.c"))
  expect_true(any(grepl("struct Flags", helpers_c) | grepl("typedef struct", helpers_c)))
  
  # Clean up
  unlink(header_file)
  unlink(pkg_dir, recursive = TRUE)
})

test_that("ABI mode (default) does not generate src/ directory", {
  skip_on_cran()
  
  # Create simple header
  header_file <- tempfile(fileext = ".h")
  writeLines(c(
    "struct Simple {",
    "    int value;",
    "};"
  ), header_file)
  
  # Generate package WITHOUT API mode
  pkg_dir <- tempfile()
  result <- generate_package_from_headers(
    header_files = header_file,
    package_name = "TestABIPkg",
    library_name = "testlib",
    output_dir = pkg_dir,
    use_api_mode = FALSE  # Explicit ABI mode
  )
  
  # Verify no src/ directory
  expect_false(dir.exists(file.path(pkg_dir, "src")))
  
  # But R/ directory should exist
  expect_true(dir.exists(file.path(pkg_dir, "R")))
  
  # Clean up
  unlink(header_file)
  unlink(pkg_dir, recursive = TRUE)
})
