# TCC Wrapper - Preprocessing utilities only
# All parsing functions removed - use tree-sitter via ffi_parse_header()

remove_c_comments <- function(code) {
  code <- gsub("(?s)/\\*.*?\\*/", " ", code, perl = TRUE)
  code <- gsub("//[^\n]*", "", code, perl = TRUE)
  code
}

#' Get path to embedded TCC binary
#' @return Path to tcc executable in installed package
#' @export
tcc_binary_path <- function() {
  if (.Platform$OS.type == "windows") {
    # On Windows, tcc.exe is in inst/tinycc/ (root of tinycc dir)
    tcc_path <- system.file("tinycc", "tcc.exe", package = "RSimpleFFI")
    if (nzchar(tcc_path) && file.exists(tcc_path)) {
      tcc_path <- normalizePath(tcc_path)
    }
  } else {
    # On Unix, tcc is in inst/tinycc/bin/
    tcc_path <- system.file("tinycc", "bin", "tcc", package = "RSimpleFFI")
    if (nzchar(tcc_path) && file.exists(tcc_path)) {
      tcc_path <- normalizePath(tcc_path)
    }
  }

  if (!nzchar(tcc_path) || !file.exists(tcc_path)) {
    stop("TCC binary not found. Package may not be installed correctly.")
  }
  # replace tcc_path with gcc/clang from R CMD confic CC
  # if we can find them
  R_CC <- system2("R", c("CMD", "config", "CC"), stdout = TRUE)
  if (nzchar(R_CC) && file.exists(R_CC)) {
    # remove stuff like 'gcc -std=gnu11' to just get the binary path
    R_CC <- strsplit(R_CC, " ")[[1]][1]
    tcc_path <- normalizePath(R_CC)
  }

  tcc_path
}

#' Preprocess C header file using embedded TCC
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

  # Diagnostic: Check TinyCC include path (differs between Windows and Unix)
  if (.Platform$OS.type == "windows") {
    # Windows: inst/tinycc/include/
    tcc_include <- file.path(dirname(tcc), "include")
  } else {
    # Unix: inst/tinycc/lib/tcc/include/
    tcc_lib_path <- file.path(dirname(dirname(tcc)), "lib", "tcc")
    tcc_include <- file.path(tcc_lib_path, "include")
  }

  if (!dir.exists(tcc_include)) {
    warning("TinyCC include path not found: ", tcc_include)
  } else {
    message("TinyCC include path: ", tcc_include)
    message("TinyCC builtin headers: ", paste(list.files(tcc_include), collapse = ", "))
  }

  tmp_out <- tempfile(fileext = ".i")
  tmp_err <- tempfile(fileext = ".err")
  on.exit(unlink(c(tmp_out, tmp_err)), add = TRUE)

  args <- c("-E", "-v") # Add -v for verbose output

  if (!is.null(includes)) {
    args <- c(args, paste0("-I", includes))
  }

  args <- c(args, header_file, "-o", tmp_out)

  # Capture both stdout and stderr separately
  result <- system2(tcc, args, stdout = tmp_err, stderr = tmp_err)

  # Read error/diagnostic output
  err_output <- if (file.exists(tmp_err)) readLines(tmp_err, warn = FALSE) else character(0)

  if (length(err_output) > 0) {
    message("TCC diagnostic output:")
    message(paste(head(err_output, 20), collapse = "\n"))
  }

  if (!file.exists(tmp_out)) {
    stop("TCC preprocessing failed:\n", paste(err_output, collapse = "\n"))
  }

  # Check if output seems truncated
  file_size <- file.info(tmp_out)$size
  lines <- readLines(tmp_out, warn = FALSE)
  num_lines <- length(lines)

  # message size of the temp file  and number of characters
  message("Preprocessed file size: ", file_size, " bytes")
  message("Preprocessed file lines: ", num_lines, " lines")

  # count bytes of each line and sum them
  line_sizes <- nchar(lines, type = "bytes")
  total_chars <- sum(line_sizes)
  message("Preprocessed file total characters: ", total_chars, " characters")

  # Check for suspiciously small output
  if (file_size < 5000 && num_lines < 100) {
    warning(
      "TinyCC preprocessing produced suspiciously small output (",
      file_size, " bytes, ", num_lines, " lines). ",
      "This may indicate incomplete preprocessing."
    )
    message("Last 10 lines of preprocessed output:")
    message(paste(tail(lines, 10), collapse = "\n"))
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

#' Compile and run C code using embedded TCC
#' @param code C source code as string
#' @param args Arguments to pass to compiled program
#' @return Output from program
#' @export
tcc_run <- function(code, args = character()) {
  tcc <- tcc_binary_path()
  tmp_src <- tempfile(fileext = ".c")
  on.exit(unlink(tmp_src), add = TRUE)

  writeLines(code, tmp_src)

  tcc_args <- c("-run", tmp_src)
  if (length(args) > 0) {
    tcc_args <- c(tcc_args, "--", args)
  }

  system2(tcc, tcc_args, stdout = TRUE, stderr = TRUE)
}

#' Check if TCC is available
#' @return Logical indicating if TCC is available
#' @export
tcc_available <- function() {
  tryCatch(
    {
      path <- tcc_binary_path()
      file.exists(path)
    },
    error = function(e) FALSE
  )
}
