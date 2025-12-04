#!/usr/bin/env Rscript
#
# Generate R package bindings from htslib headers
#
# Usage:
#   Rscript tools/generate_htslib_package.R [output_dir]

library(RSimpleFFI)

args <- commandArgs(trailingOnly = TRUE)
output_dir <- if (length(args) > 0) args[1] else "/tmp/htslibFFI"

cat("=== htslib FFI Bindings Generator ===\n\n")

# Find htslib headers
htslib_dir <- "/usr/include/htslib"
if (!dir.exists(htslib_dir)) {
    stop("htslib not found. Install with: apt-get install libhts-dev")
}

# Get all header files
headers <- list.files(htslib_dir, pattern = "\\.h$", full.names = TRUE)

cat("Found", length(headers), "headers:\n")
for (h in headers) {
    cat("  -", basename(h), "\n")
}
cat("\n")

# Generate package
cat("Generating package...\n\n")

result <- generate_package_from_headers(
    header_files = headers,
    package_name = "htslibFFI",
    library_name = "libhts.so.3", # Full soname for Linux
    output_dir = output_dir,
    use_system_lib = TRUE,
    include_helpers = TRUE,
    authors_r = 'person("RSimpleFFI", "Generator", email = "auto@generated.com", role = c("aut", "cre"))',
    title = "FFI Bindings to htslib (SAM/BAM/VCF/BCF)",
    description = "Auto-generated FFI bindings to htslib for reading and writing high-throughput sequencing data formats (SAM, BAM, CRAM, VCF, BCF)."
)

cat("\n=== Generation Complete ===\n")
cat("Package created in:", normalizePath(output_dir), "\n")
cat("Generated files:\n")
for (f in result$files) {
    cat("  -", basename(f), "\n")
}

# Show binding statistics
cat("\nBinding statistics:\n")
for (name in names(result$bindings)) {
    b <- result$bindings[[name]]
    n_funcs <- if (!is.null(b$functions)) nrow(b$functions) else 0
    n_structs <- length(b$structs)
    if (n_funcs > 0 || n_structs > 0) {
        cat(sprintf("  %s: %d functions, %d structs\n", name, n_funcs, n_structs))
    }
}

# Install
cat("\n=== Installing package ===\n")
install.packages(output_dir, repos = NULL, type = "source", quiet = TRUE)

# Demo
cat("\n=== Running Demo ===\n\n")
library(htslibFFI)

# 1. Get htslib version
version_ptr <- r_hts_version()
cat("htslib version:", pointer_to_string(version_ptr), "\n\n")

# 2. Check htslib features (returns bitmask)
features <- r_hts_features()
cat("htslib features bitmask:", sprintf("0x%X", features), "\n\n")

# 3. Test flag conversion
flag <- r_bam_str2flag("PAIRED,PROPER_PAIR")
cat("BAM flags for PAIRED,PROPER_PAIR:", flag, "\n\n")

# 4. Initialize a BAM record
bam_rec <- r_bam_init1()
cat("Created BAM record:", class(bam_rec), "\n")
r_bam_destroy1(bam_rec) # Clean up
cat("Destroyed BAM record\n\n")

# 5. Show bam1_t struct info (the BAM record type)
cat("bam1_t struct size:", ffi_sizeof(bam1_t), "bytes\n")

cat("\n=== Demo Complete ===\n")
