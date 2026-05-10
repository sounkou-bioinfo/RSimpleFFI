#' Compilation Helpers for API Mode
#'
#' Compile generated C code with Rtinycc and keep it in memory.
#'
#' @name compile_helpers
#' @keywords internal
NULL

rtinycc_add_r_runtime_paths <- function(state) {
  Rtinycc::tcc_add_include_path(state, R.home("include"))
  Rtinycc::tcc_add_library_path(state, R.home("lib"))
  if (.Platform$OS.type == "windows") {
    Rtinycc::tcc_add_library_path(state, file.path(R.home(), "bin", .Platform$r_arch))
  }
  invisible(state)
}

rtinycc_prepare_r_api_source <- function(c_source) {
  c_source <- paste(c_source, collapse = "\n")
  if (!grepl("(?m)^\\s*#\\s*define\\s+_Complex\\b", c_source, perl = TRUE)) {
    c_source <- paste("#define _Complex", c_source, sep = "\n")
  }
  c_source
}

rtinycc_add_host_symbols <- function(state) {
  add_host <- get("RC_libtcc_add_host_symbols", envir = asNamespace("Rtinycc"))
  .Call(add_host, state)
}

#' Compile C code using Rtinycc
#'
#' Takes C source code, writes it to a temporary file for diagnostics, compiles it
#' in memory with `Rtinycc`, and returns a handle that can resolve callable
#' `.Call` symbols. This avoids creating package-owned TinyCC builds or temporary
#' shared libraries for API-mode helpers.
#'
#' @param c_source Character string containing C code
#' @param include_dirs Character vector of include directories (optional)
#' @param verbose Logical, print compilation output (default FALSE)
#' @return List with elements:
#'   - state: Rtinycc compilation state
#'   - path: Path to the diagnostic C source file
#'   - tmpdir: Temporary directory kept alive for the handle lifetime
#' @export
#' @keywords internal
ffi_compile_shlib <- function(c_source, include_dirs = NULL, verbose = FALSE) {
  if (!requireNamespace("Rtinycc", quietly = TRUE)) {
    stop("Rtinycc is required for API-mode helper compilation", call. = FALSE)
  }

  tmp_dir <- tempfile(pattern = "rffi_tinycc_")
  dir.create(tmp_dir, recursive = TRUE)

  c_source <- rtinycc_prepare_r_api_source(c_source)
  c_file <- file.path(tmp_dir, "helpers.c")
  writeLines(c_source, c_file)

  if (verbose) {
    message("C source written to: ", c_file)
  }

  state <- Rtinycc::tcc_state(output = "memory")
  rtinycc_add_r_runtime_paths(state)

  if (!is.null(include_dirs) && length(include_dirs) > 0) {
    for (inc in include_dirs) {
      Rtinycc::tcc_add_include_path(state, inc)
    }
  }

  Rtinycc::tcc_add_library(state, "R")

  result <- Rtinycc::tcc_compile_string(state, c_source)
  if (!identical(result, 0L)) {
    stop("Rtinycc compilation failed for API-mode helpers. Source file: ", c_file, call. = FALSE)
  }

  rtinycc_add_host_symbols(state)

  result <- Rtinycc::tcc_relocate(state)
  if (!identical(result, 0L)) {
    stop("Rtinycc relocation failed for API-mode helpers. Source file: ", c_file, call. = FALSE)
  }

  structure(
    list(
      state = state,
      path = c_file,
      tmpdir = tmp_dir
    ),
    class = "rffi_compiled_lib"
  )
}

#' Get function pointer from compiled library
#'
#' Extract a function pointer from an Rtinycc-compiled helper object for use
#' with `.Call()`.
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

  sym_addr <- Rtinycc::tcc_get_symbol(lib$state, symbol_name)
  if (!Rtinycc::tcc_symbol_is_valid(sym_addr)) {
    stop("Symbol not found: ", symbol_name)
  }
  state <- lib$state
  force(state)
  return(function(...) {
    force(state)
    .Call(sym_addr, ...)
  })
}

#' Clean up compiled library
#'
#' Remove diagnostic temporary files. Rtinycc memory is owned by the external
#' pointer state and is released by R's garbage collector.
#'
#' @param lib Compiled library from ffi_compile_shlib()
#' @return NULL (invisible)
#' @export
#' @keywords internal
ffi_cleanup_lib <- function(lib) {
  if (!inherits(lib, "rffi_compiled_lib")) {
    stop("lib must be from ffi_compile_shlib()")
  }


  if (!is.null(lib$tmpdir) && dir.exists(lib$tmpdir)) {
    unlink(lib$tmpdir, recursive = TRUE)
  }

  invisible(NULL)
}

#' Print method for compiled library
#' @export
print.rffi_compiled_lib <- function(x, ...) {
  cat("<RFFI Compiled Library>\n")
  cat("  Backend: Rtinycc\n")
  cat("  Source:", x$path, "\n")
  cat("  Temp dir:", x$tmpdir, "\n")
  invisible(x)
}
