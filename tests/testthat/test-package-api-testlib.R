test_that("API mode package generation works with testlib", {
  skip_on_cran()
  skip_if_not_installed("RSimpleFFI")
  
  # Get paths to testlib
  testlib_dir <- system.file("extdata", "testlib", package = "RSimpleFFI")
  skip_if(!dir.exists(testlib_dir), "testlib directory not found")
  
  header_file <- file.path(testlib_dir, "testlib.h")
  lib_file <- file.path(testlib_dir, "libtestlib.so")
  
  # Build testlib if not present
  if (!file.exists(lib_file)) {
    system(sprintf("cd %s && make clean && make", testlib_dir))
  }
  
  skip_if(!file.exists(header_file), "testlib.h not found")
  skip_if(!file.exists(lib_file), "libtestlib.so not found")
  
  # Generate package with API mode
  pkg_dir <- tempfile()
  result <- generate_package_from_headers(
    header_files = header_file,
    package_name = "TestLibFFI",
    library_name = "testlib",
    output_dir = pkg_dir,
    use_api_mode = TRUE,
    library_path = testlib_dir,
    use_system_lib = FALSE,
    authors_r = 'person("Test", "Author", email = "test@example.com", role = c("aut", "cre"))'
  )
  
  # Check package structure
  expect_true(dir.exists(pkg_dir))
  expect_true(dir.exists(file.path(pkg_dir, "R")))
  expect_true(dir.exists(file.path(pkg_dir, "src")))
  
  # Check required files exist
  expect_true(file.exists(file.path(pkg_dir, "DESCRIPTION")))
  expect_true(file.exists(file.path(pkg_dir, "NAMESPACE")))
  
  # Check src/ files for API mode
  expect_true(file.exists(file.path(pkg_dir, "src", "init.c")))
  expect_true(file.exists(file.path(pkg_dir, "src", "struct_helpers.c")))
  expect_true(file.exists(file.path(pkg_dir, "src", "Makevars")))
  
  # Check R files for struct wrappers
  expect_true(file.exists(file.path(pkg_dir, "R", "Point_api.R")))
  expect_true(file.exists(file.path(pkg_dir, "R", "Rectangle_api.R")))
  expect_true(file.exists(file.path(pkg_dir, "R", "Config_api.R")))
  expect_true(file.exists(file.path(pkg_dir, "R", "FileHandle_api.R")))
  
  # Verify init.c registers all struct helpers
  init_c <- readLines(file.path(pkg_dir, "src", "init.c"))
  expect_true(any(grepl("R_init_TestLibFFI", init_c)))
  expect_true(any(grepl("rffi_Point_new", init_c)))
  expect_true(any(grepl("rffi_Config_new", init_c)))
  expect_true(any(grepl("rffi_FileHandle_new", init_c)))
  
  # Verify struct_helpers.c uses offsetof for all structs
  helpers_c <- readLines(file.path(pkg_dir, "src", "struct_helpers.c"))
  expect_true(any(grepl("offsetof.*Point.*x", helpers_c)))
  expect_true(any(grepl("offsetof.*Config", helpers_c)))
  expect_true(any(grepl("offsetof.*FileHandle", helpers_c)))
  
  # Verify R wrappers have API mode functions
  point_api <- readLines(file.path(pkg_dir, "R", "Point_api.R"))
  expect_true(any(grepl("Point_new <- function", point_api)))
  expect_true(any(grepl("Point_get <- function", point_api)))
  expect_true(any(grepl("Point_set <- function", point_api)))
  
  config_api <- readLines(file.path(pkg_dir, "R", "Config_api.R"))
  expect_true(any(grepl("Config_new <- function", config_api)))
  expect_true(any(grepl("Config_get <- function", config_api)))
  
  # Try to build the package
  build_dir <- tempfile()
  dir.create(build_dir)
  build_log <- file.path(build_dir, "build.log")
  
  old_wd <- setwd(build_dir)
  on.exit(setwd(old_wd), add = TRUE)
  build_result <- system2(
    "R", 
    c("CMD", "build", shQuote(pkg_dir)),
    stdout = build_log,
    stderr = build_log
  )
  setwd(old_wd)
  
  expect_equal(build_result, 0, 
    info = sprintf("Package build failed. Check %s", build_log))
  
  # Find the built tarball
  tarball <- list.files(build_dir, pattern = "^TestLibFFI.*\\.tar\\.gz$", full.names = TRUE)
  expect_true(length(tarball) > 0, "Package tarball not created")
  
  if (length(tarball) > 0) {
    # Try to install the package
    install_dir <- tempfile()
    dir.create(install_dir)
    
    install_result <- system2(
      "R",
      c("CMD", "INSTALL", paste0("--library=", shQuote(install_dir)), shQuote(tarball[1])),
      stdout = TRUE,
      stderr = TRUE
    )
    
    # Check if installation succeeded
    pkg_installed <- dir.exists(file.path(install_dir, "TestLibFFI"))
    expect_true(pkg_installed, "Package installation failed")
    
    # Clean up tarball and install dir
    unlink(tarball)
    unlink(install_dir, recursive = TRUE)
  }
  
  # Clean up
  unlink(build_dir, recursive = TRUE)
  unlink(pkg_dir, recursive = TRUE)
})

test_that("testlib API mode package can handle bitfield structs", {
  skip_on_cran()
  skip_if_not_installed("RSimpleFFI")
  
  testlib_dir <- system.file("extdata", "testlib", package = "RSimpleFFI")
  skip_if(!dir.exists(testlib_dir))
  
  header_file <- file.path(testlib_dir, "testlib.h")
  skip_if(!file.exists(header_file))
  
  # Generate package focusing on Config struct (has bitfields)
  pkg_dir <- tempfile()
  result <- generate_package_from_headers(
    header_files = header_file,
    package_name = "TestConfigAPI",
    library_name = "testlib",
    output_dir = pkg_dir,
    use_api_mode = TRUE,
    library_path = testlib_dir,
    use_system_lib = FALSE,
    authors_r = 'person("Test", "Author", email = "test@example.com", role = c("aut", "cre"))'
  )
  
  # Verify Config struct handling
  helpers_c <- readLines(file.path(pkg_dir, "src", "struct_helpers.c"))
  
  # Should have Config in the helpers (bitfields ARE handled in API mode)
  expect_true(any(grepl("Config", helpers_c)))
  
  # Should use offsetof for Config fields
  expect_true(any(grepl("offsetof", helpers_c)))
  
  # Config_api.R should exist and have accessors
  config_api_file <- file.path(pkg_dir, "R", "Config_api.R")
  expect_true(file.exists(config_api_file))
  
  config_api <- readLines(config_api_file)
  expect_true(any(grepl("Config_new", config_api)))
  expect_true(any(grepl("Config_get", config_api)))
  expect_true(any(grepl("Config_set", config_api)))
  
  # Clean up
  unlink(pkg_dir, recursive = TRUE)
})
