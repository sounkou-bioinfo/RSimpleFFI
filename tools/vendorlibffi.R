#!/usr/bin/env Rscript
# vendor libffi: download or unpack libffi tarball

args <- commandArgs(trailingOnly = TRUE)
mode <- if (length(args) > 0) args[[1]] else "download"

url <- "https://github.com/libffi/libffi/releases/download/v3.5.2/libffi-3.5.2.tar.gz"
version <- basename(url) |>
  gsub("^v", "", x = _) |>
  gsub("\\.tar\\.gz$", "", x = _)

dest_dir <- "../src"
tarball_path <- "../src/libffi-3.5.2.tar.gz"

getScriptPath <- function() {
  cmd.args <- commandArgs()
  m <- regexpr("(?<=^--file=).+", cmd.args, perl = TRUE)
  script.dir <- dirname(regmatches(cmd.args, m))
  if (length(script.dir) == 0) {
    stop("can't determine script dir: please call the script with Rscript")
  }
  if (length(script.dir) > 1) {
    stop("can't determine script dir: more than one '--file' argument detected")
  }
  return(script.dir)
}
script_dir <- getScriptPath()
dest_dir <- file.path(script_dir, dest_dir) |> normalizePath()
tarball_path <- file.path(script_dir, tarball_path)

if (mode == "download") {
  message("Downloading libffi tarball to ", tarball_path)
  download.file(url, tarball_path, quiet = TRUE)
  message("libffi tarball downloaded.")
  quit(save = "no")
}

if (mode == "unpack") {
  if (!file.exists(tarball_path)) {
    stop("libffi tarball not found: ", tarball_path)
  }
  # Remove any existing libffi* directories in dest_dir
  old_dirs <- list.dirs(dest_dir, full.names = TRUE, recursive = FALSE)
  old_dirs <- old_dirs[grepl("^libffi", basename(old_dirs))]
  if (length(old_dirs) > 0) {
    unlink(old_dirs, recursive = TRUE)
  }
  utils::untar(tarball_path, exdir = dest_dir)
  # Find the extracted directory (should match libffi*)
  new_dirs <- list.dirs(dest_dir, full.names = TRUE, recursive = FALSE)
  libffi_dir <- new_dirs[grepl("^libffi", basename(new_dirs))]
  if (length(libffi_dir) == 0) {
    stop("Could not find extracted libffi directory in ", dest_dir)
  }
  # If not already named 'libffi', rename it
  if (!basename(libffi_dir[1]) == "libffi") {
    file.rename(libffi_dir[1], file.path(dest_dir, "libffi"))
    libffi_dir <- file.path(dest_dir, "libffi")
  }
  message("libffi unpacked to ", libffi_dir)
  quit(save = "no")
}

stop("Unknown mode: ", mode)
