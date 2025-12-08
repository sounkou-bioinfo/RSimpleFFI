#' Compilation Helpers for API Mode
#'
#' Compile generated C code using R CMD SHLIB and load the resulting shared library.
#'
#' @name compile_helpers
#' @keywords internal
NULL

#' Compile C code using R CMD SHLIB
#'
#' Takes C source code, writes it to a temporary file, compiles it with R CMD SHLIB,
#' and loads the resulting shared library. Uses the system compiler (gcc/clang/MSVC)
#' for reliability and full language support.
#'
#' @param c_source Character string containing C code
#' @param include_dirs Character vector of include directories (optional)
#' @param verbose Logical, print compilation output (default FALSE)
#' @return List with elements:
#'   - dll: DLL handle from dyn.load()
#'   - path: Path to the compiled .so/.dll file
#'   - tmpdir: Temporary directory (kept alive for DLL lifetime)
#' @export
#' @keywords internal
ffi_compile_shlib <- function(c_source, include_dirs = NULL, verbose = FALSE) {
  # Create temp directory
  tmp_dir <- tempfile(pattern = "rffi_shlib_")
  dir.create(tmp_dir, recursive = TRUE)
  
  # Write C source
  c_file <- file.path(tmp_dir, "helpers.c")
  writeLines(c_source, c_file)
  
  if (verbose) {
    message("C source written to: ", c_file)
  }
  
  # Build compilation command
  old_wd <- getwd()
  on.exit(setwd(old_wd), add = TRUE)
  setwd(tmp_dir)
  
  # Set up PKG_CPPFLAGS if we have include directories
  if (!is.null(include_dirs) && length(include_dirs) > 0) {
    includes <- paste0("-I", include_dirs, collapse = " ")
    Sys.setenv(PKG_CPPFLAGS = includes)
    on.exit(Sys.unsetenv("PKG_CPPFLAGS"), add = TRUE)
  }
  
  # Compile with R CMD SHLIB
  r_bin <- file.path(R.home("bin"), "R")
  cmd <- c("CMD", "SHLIB", basename(c_file))
  
  if (verbose) {
    message("Running: ", r_bin, " ", paste(cmd, collapse = " "))
    result <- system2(r_bin, cmd, stdout = "", stderr = "")
  } else {
    result <- system2(r_bin, cmd, stdout = FALSE, stderr = FALSE)
  }
  
  if (result != 0) {
    # Try again with verbose output for error message
    result_verbose <- system2(r_bin, cmd, stdout = TRUE, stderr = TRUE)
    stop("R CMD SHLIB compilation failed:\n", 
         paste(result_verbose, collapse = "\n"))
  }
  
  # Find compiled library
  lib_ext <- if (.Platform$OS.type == "windows") "\\.dll$" else "\\.so$"
  so_files <- list.files(tmp_dir, pattern = lib_ext, full.names = TRUE)
  
  if (length(so_files) == 0) {
    stop("Compilation did not produce a shared library")
  }
  
  so_file <- so_files[1]
  
  if (verbose) {
    message("Compiled library: ", so_file)
  }
  
  # Load library
  dll <- dyn.load(so_file)
  
  # Return handle with tmpdir to keep it alive
  structure(
    list(
      dll = dll,
      path = so_file,
      tmpdir = tmp_dir
    ),
    class = "rffi_compiled_lib"
  )
}

#' Get function pointer from compiled library
#'
#' Extract a function pointer from a compiled library for use with .Call().
#'
#' @param lib Compiled library from ffi_compile_shlib()
#' @param symbol_name Name of the C function
#' @return Function that calls the C function via .Call()
#' @export
#' @keywords internal
ffi_get_symbol <- function(lib, symbol_name) {
  if (!inherits(lib, "rffi_compiled_lib")) {
    stop("lib must be from ffi_compile_shlib()")
  }
  
  # Check if symbol exists
  addr <- tryCatch(
    getNativeSymbolInfo(symbol_name, lib$dll),
    error = function(e) NULL
  )
  
  if (is.null(addr)) {
    stop("Symbol not found: ", symbol_name)
  }
  
  # Return a wrapper function that uses .Call with the symbol address
  # We need to use the address directly, not the DLL
  function(...) {
    .Call(addr$address, ...)
  }
}

#' Clean up compiled library
#'
#' Unload library and remove temporary files.
#'
#' @param lib Compiled library from ffi_compile_shlib()
#' @return NULL (invisible)
#' @export
#' @keywords internal
ffi_cleanup_lib <- function(lib) {
  if (!inherits(lib, "rffi_compiled_lib")) {
    stop("lib must be from ffi_compile_shlib()")
  }
  
  # Unload library
  if (!is.null(lib$dll)) {
    tryCatch(dyn.unload(lib$path), error = function(e) {
      warning("Failed to unload library: ", e$message)
    })
  }
  
  # Remove temp directory
  if (!is.null(lib$tmpdir) && dir.exists(lib$tmpdir)) {
    unlink(lib$tmpdir, recursive = TRUE)
  }
  
  invisible(NULL)
}

#' Print method for compiled library
#' @export
print.rffi_compiled_lib <- function(x, ...) {
  cat("<RFFI Compiled Library>\n")
  cat("  Path:", x$path, "\n")
  cat("  Temp dir:", x$tmpdir, "\n")
  
  # List symbols
  symbols <- tryCatch({
    info <- getDLLRegisteredRoutines(x$dll[[1]])
    c(names(info$.Call), names(info$.C))
  }, error = function(e) character(0))
  
  if (length(symbols) > 0) {
    cat("  Symbols:", paste(head(symbols, 5), collapse = ", "))
    if (length(symbols) > 5) cat(", ...")
    cat("\n")
  }
  
  invisible(x)
}
