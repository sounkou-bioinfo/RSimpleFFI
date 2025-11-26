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
    if (dir.exists(file.path(dest_dir, paste0("libffi-", version)))) {
        unlink(file.path(dest_dir, paste0("libffi-", version)), recursive = TRUE)
    }
    utils::untar(tarball_path, exdir = dest_dir)
    # rename libffi-version to libffi
    if (!dir.exists(file.path(dest_dir, "libffi"))) {
        dir.create(file.path(dest_dir, "libffi"), recursive = TRUE)
    }
    file.rename(
        file.path(dest_dir, paste0("libffi-", version)),
        file.path(dest_dir, "libffi")
    )
    message("libffi unpacked to ", file.path(dest_dir, "libffi"))
    quit(save = "no")
}

stop("Unknown mode: ", mode)
