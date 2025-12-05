#!/usr/bin/env Rscript
#
# Demo: VCF Processing with htslib via RSimpleFFI
#
# Usage:
#   Rscript tools/demo_htslib_vcf.R

library(RSimpleFFI)

message("=== VCF Processing Demo with RSimpleFFI ===\n")

# Check if htslib is available
if (!file.exists("/usr/include/htslib/vcf.h")) {
  stop("htslib not found. Install with: sudo apt-get install libhts-dev")
}

# Generate bindings if needed
if (!require(htslibFFI, quietly = TRUE)) {
  message("Generating htslibFFI package...")
  source("tools/generate_htslib_package.R")
  library(htslibFFI)
}

message("Loaded htslibFFI bindings")
message("htslib version: ", pointer_to_string(r_hts_version()), "\n")

# Demo 1: Create and manipulate BAM records
demo_bam_records <- function() {
  message("=== Demo 1: BAM Record Manipulation ===\n")
  
  # Create BAM record
  message("Creating BAM record...")
  rec <- r_bam_init1()
  message("  Record pointer: ", class(rec))
  
  # Get BAM flag names
  message("\nTesting BAM flag conversions:")
  flag <- r_bam_str2flag("PAIRED,PROPER_PAIR")
  message("  PAIRED,PROPER_PAIR = ", flag)
  
  flag2 <- r_bam_str2flag("UNMAP,MUNMAP")
  message("  UNMAP,MUNMAP = ", flag2)
  
  # Clean up
  r_bam_destroy1(rec)
  message("  Destroyed BAM record\n")
}

# Demo 2: Check htslib features
demo_htslib_features <- function() {
  message("=== Demo 2: htslib Features ===\n")
  
  features <- r_hts_features()
  message("Feature bitmask: 0x", sprintf("%X", features))
  
  # Decode features
  if (bitwAnd(features, 1) != 0) message("  ✓ CRAM support")
  if (bitwAnd(features, 2) != 0) message("  ✓ GCS support") 
  if (bitwAnd(features, 4) != 0) message("  ✓ S3 support")
  if (bitwAnd(features, 256) != 0) message("  ✓ libcurl support")
  
  message()
}

# Demo 3: Read VCF from 1000 Genomes (streaming from URL)
demo_read_vcf_url <- function() {
  message("=== Demo 3: Reading VCF from 1000 Genomes Project ===\n")
  
  # Use chr21 phased variants (smaller file)
  vcf_url <- "https://ftp.1000genomes.ebi.ac.uk/vol1/ftp/data_collections/1000G_2504_high_coverage/working/20220422_3202_phased_SNV_INDEL_SV/1kGP_high_coverage_Illumina.chr21.filtered.SNV_INDEL_SV_phased_panel.vcf.gz"
  
  message("Opening VCF from URL (htslib streams without download):")
  message("  ", vcf_url, "\n")
  
  # Open VCF file directly from URL
  fp <- r_hts_open(vcf_url, "r")
  if (is_null_pointer(fp)) {
    message("  ✗ Could not open URL (may need libcurl support)")
    message("  Note: htslib can stream from URLs if built with libcurl\n")
    return()
  }
  
  message("  ✓ Opened successfully\n")
  
  # Read header
  hdr <- r_bcf_hdr_read(fp)
  if (is_null_pointer(hdr)) {
    message("  ✗ Could not read VCF header\n")
    r_hts_close(fp)
    return()
  }
  message("  ✓ Header read successfully\n")
  
  # Read first 10 variants
  message("Reading first 3 variants:\n\n")
  rec <- r_bcf_init()
  
  # Open temp file to write VCF output
  tmpfile <- tempfile(fileext = ".vcf")
  outfp <- r_hts_open(tmpfile, "w")
  
  # Write header
  r_vcf_hdr_write(outfp, hdr)
  
  count <- 0
  while (count < 3) {  # Just show 3 for demo
    ret <- r_bcf_read(fp, hdr, rec)
    if (ret < 0) break  # EOF or error
    
    # Unpack variant to access all fields
    r_bcf_unpack(rec, 15L)  # BCF_UN_ALL = 15 (unpack everything)
    
    # Write to temp VCF file
    r_vcf_write(outfp, hdr, rec)
    
    count <- count + 1
  }
  
  r_hts_close(outfp)
  
  message(sprintf("✓ Read %d variants from URL!\n", count))
  message("\nVCF records written to temp file. Content:\n")
  
  # Read and display the VCF lines
  vcf_content <- readLines(tmpfile)
  for (line in vcf_content[-(1:grep("^#CHROM", vcf_content))]) {
    if (nchar(line) > 0) {
      # Truncate long lines for display
      display_line <- if (nchar(line) > 120) {
        paste0(substr(line, 1, 117), "...")
      } else {
        line
      }
      message(display_line)
    }
  }
  unlink(tmpfile)
  
  # Cleanup
  r_bcf_destroy(rec)
  r_bcf_hdr_destroy(hdr)
  r_hts_close(fp)
  
  message("This demonstrates RSimpleFFI + htslib with libcurl streaming genomics data!")
}

# Run all demos
message("Running actual htslib function calls...\n")
demo_bam_records()
demo_htslib_features()
demo_read_vcf_url()
message("=== Demo Complete ===\n")