#!/usr/bin/env Rscript
#
# Generate R package bindings from htslib headers
#
# Usage:
#   Rscript tools/generate_htslib_package.R [output_dir] [htslib_root]
#
# Arguments:
#   output_dir   - Directory for generated package (default: /tmp/htslibFFI)
#   htslib_root  - Root of custom htslib installation (default: /usr)
#                  Headers expected at: <htslib_root>/include/htslib
#                  Library expected at: <htslib_root>/lib/libhts.so

library(RSimpleFFI)

args <- commandArgs(trailingOnly = TRUE)
output_dir <- if (length(args) > 0) args[1] else "/tmp/htslibFFI"
htslib_root <- if (length(args) > 1) args[2] else "/usr"

message("=== htslib FFI Bindings Generator ===\n\n")

# Find htslib headers
htslib_dir <- file.path(htslib_root, "include", "htslib")
if (!dir.exists(htslib_dir)) {
    stop("htslib headers not found at: ", htslib_dir, "\n",
         "Install followind instructions here http://www.htslib.org/download/\n",
         "Or specify custom htslib root as second argument")
}

# Determine library location
lib_dir <- file.path(htslib_root, "lib")
lib_dir64 <- file.path(htslib_root, "lib64")
if (dir.exists(lib_dir64)) {
    lib_path <- lib_dir64
} else if (dir.exists(lib_dir)) {
    lib_path <- lib_dir
} else {
    stop("Library directory not found at: ", lib_dir, " or ", lib_dir64)
}

# Find the actual library file
# platform dynlib extensions
ext <- .Platform$dynlib.ext
lib_files <- list.files(lib_path, pattern = paste0("^libhts\\", ext), full.names = TRUE)
# macos
if (length(lib_files) == 0) {
    stop("libhts.so not found in: ", lib_path)
}
library_name <- basename(lib_files[1])

message("Using htslib installation:")
message("  Root:     ", htslib_root)
message("  Headers:  ", htslib_dir)
message("  Library:  ", file.path(lib_path, library_name))
message()

# Get all header files
headers <- list.files(htslib_dir, pattern = "\\.h$", full.names = TRUE)

message("Found ", length(headers), " headers:\n")
for (h in headers) {
    message("  - ", basename(h), "\n")
}
message("\n")

# Generate package
message("Generating package...\n\n")

# Determine if using system library or custom
use_system_lib <- (htslib_root == "/usr")

# For custom installations, use full library path
full_lib_path <- if (!use_system_lib) {
    file.path(lib_path, library_name)
} else {
    NULL
}

result <- generate_package_from_headers(
    header_files = headers,
    package_name = "htslibFFI",
    library_name = library_name,
    library_path = full_lib_path,
    output_dir = output_dir,
    use_system_lib = use_system_lib,
    include_helpers = TRUE,
    authors_r = 'person("RSimpleFFI", "Generator", email = "auto@generated.com", role = c("aut", "cre"))',
    title = "FFI Bindings to htslib (SAM/BAM/VCF/BCF)",
    description = "Auto-generated FFI bindings to htslib for reading and writing high-throughput sequencing data formats (SAM, BAM, CRAM, VCF, BCF)."
)

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
    if (n_funcs > 0 || n_structs > 0) {
        message(sprintf("  %s: %d functions, %d structs\n", name, n_funcs, n_structs))
    }
}

# Install
message("\n=== Installing package ===\n")
install.packages(output_dir, repos = NULL, type = "source", quiet = TRUE)

# Demo
message("\n=== Running Demo ===\n\n")
library(htslibFFI)

# 1. Get htslib version
version_ptr <- r_hts_version()
message("htslib version:", pointer_to_string(version_ptr), "\n\n")

# 2. Check htslib features (returns bitmask)
features <- r_hts_features()
message("htslib features bitmask:", sprintf("0x%X", features), "\n\n")

# 3. Test flag conversion
flag <- r_bam_str2flag("PAIRED,PROPER_PAIR")
message("BAM flags for PAIRED,PROPER_PAIR:", flag, "\n\n")

# 4. Initialize a BAM record
bam_rec <- r_bam_init1()
message("Created BAM record:", class(bam_rec), "\n")
r_bam_destroy1(bam_rec) # Clean up
message("Destroyed BAM record\n\n")

# 5. Show bam1_t struct info (the BAM record type)
message("bam1_t struct size:", ffi_sizeof(bam1_t), "bytes\n")

message("\n=== Demo Complete ===\n")
