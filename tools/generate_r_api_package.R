#!/usr/bin/env Rscript
#
# Generate R package bindings from R's system headers
#
# Usage:
#   Rscript tools/generate_r_api_package.R [output_dir]
#
# This script uses RSimpleFFI's built-in package generation functions
# to create FFI bindings for R's C API headers.
# If no output_dir is specified, generates in /tmp, installs, and runs demo.

library(RSimpleFFI)

# Configuration
args <- commandArgs(trailingOnly = TRUE)
output_dir <- if (length(args) > 0) args[1] else "/tmp/RInternalsFFI"

message("=== R API FFI Bindings Generator ===\n\n")

# Show available headers using built-in summary function
message("Checking available R headers...\n")
header_summary <- bindgen_r_api_summary()
message(
  "Found",
  sum(header_summary$exists),
  "headers in R include directory\n\n"
)

# Get R's include directory
r_include_dir <- R.home("include")

# Recursively find all header files
headers_to_parse <- list.files(
  r_include_dir,
  pattern = "\\.h$",
  recursive = TRUE,
  full.names = TRUE
)

# Filter to existing headers only
headers_to_parse <- headers_to_parse[file.exists(headers_to_parse)]

message("Will parse", length(headers_to_parse), "headers:\n")
for (h in headers_to_parse) {
  message("  -", sub(r_include_dir, "", h), "\n")
}
message("\n")

# Use RSimpleFFI's generate_package_from_headers function
message("Generating package using generate_package_from_headers()...\n\n")

result <- generate_package_from_headers(
  header_files = headers_to_parse,
  package_name = "RInternalsFFI",
  library_name = "R",
  output_dir = output_dir,
  use_system_lib = FALSE,
  include_helpers = TRUE,
  authors_r = 'person("RSimpleFFI", "Generator", email = "auto@generated.com", role = c("aut", "cre"))',
  title = "FFI Bindings to R's Internal C API",
  description = "Auto-generated FFI bindings to R's internal C API functions. Provides direct access to functions from Rinternals.h, Rmath.h, and other R system headers via the RSimpleFFI package."
)

# Custom zzz.R for R API (R symbols are already loaded in the process)
zzz_code <- '
# Package initialization for R API bindings

.onLoad <- function(libname, pkgname) {
  # R symbols are already available in the running R process
  # No additional library loading needed on Unix/macOS

  if (.Platform$OS.type == "windows") {
    # On Windows, ensure R.dll symbols are available
    r_dll <- file.path(R.home("bin"), .Platform$r_arch, "R.dll")
    if (!file.exists(r_dll)) {
      r_dll <- file.path(R.home("bin"), "R.dll")
    }
    if (file.exists(r_dll)) {
      trymessagech(
        RSimpleFFI::dll_load(r_dll),
        error = function(e) {
          # R.dll may already be loaded
        }
      )
    }
  }
}

.onUnload <- function(libpath) {
  # Cleanup if needed
}
'
zzz_file <- file.path(output_dir, "R", "zzz.R")
writeLines(zzz_code, zzz_file)
message("Updated zzz.R for R API bindings\n")

# Print summary
message("\n=== Generation Complete ===\n")
message("Package created in:", normalizePath(output_dir), "\n")
message("Generated files:\n")
for (f in result$files) {
  message("  -", basename(f), "\n")
}

# Show binding statistics
message("\nBinding statistics:\n")
for (name in names(result$bindings)) {
  b <- result$bindings[[name]]
  n_funcs <- if (!is.null(b$functions)) nrow(b$functions) else 0
  n_structs <- length(b$structs)
  message(sprintf("  %s: %d functions, %d structs\n", name, n_funcs, n_structs))
}

# Install and run demo
message("\n=== Installing package ===\n")
install.packages(output_dir, repos = NULL, type = "source", quiet = TRUE)

message("\n=== Running Demo ===\n\n")
library(RInternalsFFI)

# Load R shared library so symbols are available
message("Loading R shared library...\n")
r_lib <- file.path(
  R.home("lib"),
  if (.Platform$OS.type == "windows") "R.dll" else "libR.so"
)
if (!file.exists(r_lib) && Sys.info()["sysname"] == "Darwin") {
  r_lib <- file.path(R.home("lib"), "libR.dylib")
}
dll_load(r_lib)
message("Loaded:", r_lib, "\n\n")

# Demo: Use generated bindings
message("Demo using generated bindings:\n\n")

# Memory allomessageion from R_ext/Memory.h
message("R Memory API:\n")
ptr <- r_R_alloc(10L, 8L)
message("  r_R_alloc(10, 8): allomessageed pointer\n")

# Graphics Engine API
message("\nR Graphics Engine API:\n")
message("  r_R_GE_getVersion():", r_R_GE_getVersion(0L), "\n")

message("\n=== Demo Complete ===\n")
