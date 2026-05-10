# TCC Wrapper - Preprocessing utilities only
# Parsing functions use tree-sitter via ffi_parse_header(). TinyCC itself is
# provided by Rtinycc so RSimpleFFI does not build or vendor its own TinyCC.

#' @importFrom utils tail
NULL

remove_c_comments <- function(code) {
  code <- gsub("(?s)/\\*.*?\\*/", " ", code, perl = TRUE)
  code <- gsub("//[^\n]*", "", code, perl = TRUE)
  code
}

rtinycc_loader_env <- function() {
  lib_paths <- Rtinycc::tcc_lib_paths()
  sysname <- Sys.info()[["sysname"]]
  env_key <- switch(
    sysname,
    "Darwin" = "DYLD_LIBRARY_PATH",
    "Windows" = "PATH",
    "LD_LIBRARY_PATH"
  )
  env_sep <- if (.Platform$OS.type == "windows") ";" else ":"
  sprintf(
    "%s=%s",
    env_key,
    paste(c(lib_paths, Sys.getenv(env_key)), collapse = env_sep)
  )
}

#' Get path to the TinyCC binary supplied by Rtinycc
#' @return Path to tcc executable from the Rtinycc package
#' @export
tcc_binary_path <- function() {
  if (!requireNamespace("Rtinycc", quietly = TRUE)) {
    stop("Rtinycc is required for TinyCC support.", call. = FALSE)
  }

  tcc_path <- Rtinycc::tcc_path()
  if (!nzchar(tcc_path) || !file.exists(tcc_path)) {
    stop("TinyCC binary not found via Rtinycc. Is Rtinycc installed correctly?", call. = FALSE)
  }

  normalizePath(tcc_path, winslash = "/", mustWork = TRUE)
}

#' Preprocess C header file using TinyCC from Rtinycc
#' @param header_file Path to C header file
#' @param includes Additional include directories
#' @param keep_defines Keep #define directives (not supported by -E alone)
#' @return Character vector of preprocessed lines
#' @export
tcc_preprocess <- function(header_file, includes = NULL, keep_defines = FALSE) {
  if (!file.exists(header_file)) {
    stop("Header file not found: ", header_file)
  }

  tcc <- tcc_binary_path()
  tcc_includes <- Rtinycc::tcc_include_paths()
  tcc_prefix <- Rtinycc::tcc_prefix()

  if (length(tcc_includes) > 0) {
    message("Rtinycc TinyCC include paths: ", paste(tcc_includes, collapse = ", "))
  }

  tmp_out <- tempfile(fileext = ".i")
  tmp_err <- tempfile(fileext = ".err")
  on.exit(unlink(c(tmp_out, tmp_err)), add = TRUE)

  args <- c("-E", "-v", "-B", tcc_prefix)

  if (length(tcc_includes) > 0) {
    args <- c(args, paste0("-I", tcc_includes))
  }

  if (!is.null(includes)) {
    args <- c(args, paste0("-I", includes))
  }

  args <- c(args, header_file, "-o", tmp_out)

  # Capture both stdout and stderr separately. TinyCC diagnostics normally go
  # to stderr; some builds may emit verbose output to stdout.
  result <- system2(
    tcc,
    args,
    stdout = tmp_err,
    stderr = tmp_err,
    env = rtinycc_loader_env()
  )

  err_output <- if (file.exists(tmp_err)) readLines(tmp_err, warn = FALSE) else character(0)

  if (length(err_output) > 0) {
    message("TinyCC diagnostic output:")
    message(paste(head(err_output, 20), collapse = "\n"))
  }

  if (!identical(result, 0L)) {
    stop("TinyCC preprocessing failed with exit status ", result, ":\n", paste(err_output, collapse = "\n"))
  }

  if (!file.exists(tmp_out)) {
    stop("TinyCC preprocessing failed: no output file was created.\n", paste(err_output, collapse = "\n"))
  }

  file_size <- file.info(tmp_out)$size
  lines <- readLines(tmp_out, warn = FALSE)
  num_lines <- length(lines)

  message("Preprocessed file size: ", file_size, " bytes")
  message("Preprocessed file lines: ", num_lines, " lines")

  line_sizes <- nchar(lines, type = "bytes")
  total_chars <- sum(line_sizes)
  message("Preprocessed file total characters: ", total_chars, " characters")

  if (file_size < 5000 && num_lines < 100) {
    warning(
      "TinyCC preprocessing produced suspiciously small output (",
      file_size, " bytes, ", num_lines, " lines). ",
      "This may indicate incomplete preprocessing."
    )
    message("Last 10 lines of preprocessed output:")
    message(paste(utils::tail(lines, 10), collapse = "\n"))
  }

  lines
}

#' Extract #define macros from C header file or preprocessed lines
#' @param header_file Path to C header file (optional if preprocessed_lines provided)
#' @param preprocessed_lines Character vector from tcc_preprocess() (optional)
#' @return Named list of macro definitions
#' @export
tcc_extract_defines <- function(header_file = NULL, preprocessed_lines = NULL) {
  defines <- list()

  extract_defines_from_content <- function(content) {
    result <- list()
    content <- remove_c_comments(content)
    lines <- strsplit(content, "\n")[[1]]
    for (line in lines) {
      if (grepl("^\\s*#define\\s+", line) && !grepl("\\(", line)) {
        content_part <- sub("^\\s*#define\\s+", "", line)
        parts <- strsplit(trimws(content_part), "\\s+")[[1]]
        if (length(parts) >= 1) {
          name <- parts[1]
          value <- if (length(parts) > 1) {
            paste(parts[-1], collapse = " ")
          } else {
            ""
          }
          result[[name]] <- trimws(value)
        }
      }
    }
    result
  }

  if (!is.null(preprocessed_lines)) {
    file_markers <- preprocessed_lines[grepl(
      "^#\\s+\\d+\\s+\"",
      preprocessed_lines
    )]
    file_pattern <- '^#\\s+\\d+\\s+"([^"]+)"'

    files_to_scan <- unique(unlist(lapply(file_markers, function(marker) {
      m <- regexec(file_pattern, marker, perl = TRUE)
      parts <- regmatches(marker, m)[[1]]
      if (length(parts) >= 2) parts[2] else NULL
    })))

    files_to_scan <- Filter(
      function(f) !is.null(f) && file.exists(f),
      files_to_scan
    )

    for (f in files_to_scan) {
      content <- tryCatch(
        paste(readLines(f, warn = FALSE), collapse = "\n"),
        error = function(e) ""
      )
      if (nzchar(content)) {
        file_defines <- extract_defines_from_content(content)
        defines <- c(defines, file_defines)
      }
    }
  }

  if (!is.null(header_file)) {
    if (!file.exists(header_file)) {
      stop("Header file not found: ", header_file)
    }
    content <- paste(readLines(header_file, warn = FALSE), collapse = "\n")
    file_defines <- extract_defines_from_content(content)
    defines <- c(defines, file_defines)
  }

  defines
}

#' Compile and run C code using TinyCC from Rtinycc
#' @param code C source code as string
#' @param args Arguments to pass to compiled program
#' @return Output from program
#' @export
tcc_run <- function(code, args = character()) {
  tcc <- tcc_binary_path()
  tmp_src <- tempfile(fileext = ".c")
  on.exit(unlink(tmp_src), add = TRUE)

  writeLines(code, tmp_src)

  tcc_args <- c(
    "-B",
    Rtinycc::tcc_prefix(),
    paste0("-I", Rtinycc::tcc_include_paths()),
    "-run",
    tmp_src
  )
  if (length(args) > 0) {
    tcc_args <- c(tcc_args, "--", args)
  }

  system2(tcc, tcc_args, stdout = TRUE, stderr = TRUE, env = rtinycc_loader_env())
}

#' Check if TinyCC is available through Rtinycc
#' @return Logical indicating if TinyCC is available
#' @export
tcc_available <- function() {
  tryCatch(
    {
      requireNamespace("Rtinycc", quietly = TRUE) &&
        file.exists(Rtinycc::tcc_path())
    },
    error = function(e) FALSE
  )
}
