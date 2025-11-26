#!/usr/bin/env Rscript
# vendor libffi into src/libffi

# url https://github.com/libffi/libffi/releases/download/v3.5.2/libffi-3.5.2.tar.gz

url <- "https://github.com/libffi/libffi/releases/download/v3.5.2/libffi-3.5.2.tar.gz"
version <- basename(url) |>
    gsub("^v", "", x = _) |>
    gsub("\\.tar\\.gz$", "", x = _)

dest_dir <- "../src"
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
# get this script's directory
script_dir <- getScriptPath()
dest_dir <- file.path(script_dir, dest_dir) |> normalizePath()
message("Downloading libffi to ", dest_dir)
temp_file <- tempfile(fileext = ".tar.gz")
download.file(url, temp_file, quiet = TRUE)
if (dir.exists(file.path(dest_dir, paste0("libffi-", version)))) {
    unlink(file.path(dest_dir, paste0("libffi-", version)), recursive = TRUE)
}
utils::untar(temp_file, exdir = dest_dir)
unlink(temp_file)
# renmae libffi-version to libffi
if (!dir.exists(file.path(dest_dir, "libffi"))) {
    dir.create(file.path(dest_dir, "libffi"), recursive = TRUE)
}
file.rename(
    file.path(dest_dir, "libffi-3.5.2"),
    file.path(dest_dir, "libffi")
)
message("libffi downloaded and extracted to ", file.path(dest_dir, "libffi"))
