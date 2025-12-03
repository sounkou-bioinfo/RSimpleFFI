#' Get path to embedded TCC binary
#' @return Path to tcc executable in installed package
#' @export
tcc_binary_path <- function() {
    if(.Platform$OS.type == "windows") {
        tcc_path <- system.file("tinycc", "bin", "tcc.exe", package = "RSimpleFFI")
    } else {
        tcc_path <- system.file("tinycc", "bin", "tcc", package = "RSimpleFFI")
    }

    if (!file.exists(tcc_path) || tcc_path == "") {
        stop("TCC binary not found. Package may not be installed correctly.")
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
  tmp_out <- tempfile(fileext = ".i")
  on.exit(unlink(tmp_out), add = TRUE)
  
  # Build arguments
  args <- c("-E")
  
  if (!is.null(includes)) {
    args <- c(args, paste0("-I", includes))
  }
  
  args <- c(args, header_file, "-o", tmp_out)
  
  # Run TCC preprocessor
  result <- system2(tcc, args, stdout = TRUE, stderr = TRUE)
  
  if (!file.exists(tmp_out)) {
    stop("TCC preprocessing failed:\n", paste(result, collapse = "\n"))
  }
  
  readLines(tmp_out)
}

#' Extract #define macros from C header file or preprocessed lines
#' @param header_file Path to C header file (optional if preprocessed_lines provided)
#' @param preprocessed_lines Character vector from tcc_preprocess() (optional)
#' @return Named list of macro definitions
#' @export
tcc_extract_defines <- function(header_file = NULL, preprocessed_lines = NULL) {
  defines <- list()
  
  # If preprocessed lines provided, extract from preprocessing markers
  if (!is.null(preprocessed_lines)) {
    # TCC preprocessor doesn't preserve #define in output, so we need to parse the original file
    # Extract file paths from line markers like: # 1 "/path/to/file.h"
    file_markers <- preprocessed_lines[grepl("^#\\s+\\d+\\s+\"", preprocessed_lines)]
    file_pattern <- '^#\\s+\\d+\\s+"([^"]+)"'
    
    files_to_scan <- unique(unlist(lapply(file_markers, function(marker) {
      m <- regexec(file_pattern, marker, perl = TRUE)
      parts <- regmatches(marker, m)[[1]]
      if (length(parts) >= 2) parts[2] else NULL
    })))
    
    files_to_scan <- Filter(function(f) !is.null(f) && file.exists(f), files_to_scan)
    
    # Read and extract defines from all included files
    for (f in files_to_scan) {
      lines <- tryCatch(readLines(f, warn = FALSE), error = function(e) character())
      for (line in lines) {
        if (grepl("^\\s*#define\\s+", line) && !grepl("\\(", line)) {
          content <- sub("^\\s*#define\\s+", "", line)
          parts <- strsplit(trimws(content), "\\s+")[[1]]
          if (length(parts) >= 1) {
            name <- parts[1]
            value <- if (length(parts) > 1) paste(parts[-1], collapse = " ") else ""
            defines[[name]] <- value
          }
        }
      }
    }
  }
  
  # Also extract from main header file if provided
  if (!is.null(header_file)) {
    if (!file.exists(header_file)) {
      stop("Header file not found: ", header_file)
    }
    lines <- readLines(header_file)
    for (line in lines) {
      if (grepl("^\\s*#define\\s+", line) && !grepl("\\(", line)) {
        content <- sub("^\\s*#define\\s+", "", line)
        parts <- strsplit(trimws(content), "\\s+")[[1]]
        if (length(parts) >= 1) {
          name <- parts[1]
          value <- if (length(parts) > 1) paste(parts[-1], collapse = " ") else ""
          defines[[name]] <- value
        }
      }
    }
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
  
  # Use -run to compile and execute
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
  tryCatch({
    path <- tcc_binary_path()
    file.exists(path)
  }, error = function(e) FALSE)
}

#' Extract function declarations from preprocessed C code
#' @param preprocessed_lines Character vector from tcc_preprocess()
#' @return Data frame with columns: name, return_type, params, full_declaration
#' @export
tcc_extract_functions <- function(preprocessed_lines) {
  # Remove line markers and empty lines
  code_lines <- preprocessed_lines[!grepl("^#", preprocessed_lines)]
  code_lines <- code_lines[nzchar(trimws(code_lines))]
  
  # Join into single text for multi-line function matching
  code <- paste(code_lines, collapse = " ")
  
  # Remove comments
  code <- gsub("/\\*.*?\\*/", " ", code)
  code <- gsub("//.*?($|;)", ";", code)
  
  # Match function declarations (not definitions)
  # Pattern: return_type function_name(params);
  # Simple pattern - can be enhanced
  pattern <- "([a-zA-Z_][a-zA-Z0-9_*\\s]+)\\s+([a-zA-Z_][a-zA-Z0-9_]*)\\s*\\(([^)]*)\\)\\s*;"
  
  matches <- gregexpr(pattern, code, perl = TRUE)
  match_data <- regmatches(code, matches)[[1]]
  
  if (length(match_data) == 0) {
    return(data.frame(
      name = character(),
      return_type = character(),
      params = character(),
      full_declaration = character(),
      stringsAsFactors = FALSE
    ))
  }
  
  result <- lapply(match_data, function(decl) {
    # Extract components
    m <- regexec(pattern, decl, perl = TRUE)
    parts <- regmatches(decl, m)[[1]]
    
    if (length(parts) >= 4) {
      list(
        name = trimws(parts[3]),
        return_type = trimws(parts[2]),
        params = trimws(parts[4]),
        full_declaration = trimws(decl)
      )
    } else {
      NULL
    }
  })
  
  result <- Filter(Negate(is.null), result)
  
  if (length(result) == 0) {
    return(data.frame(
      name = character(),
      return_type = character(),
      params = character(),
      full_declaration = character(),
      stringsAsFactors = FALSE
    ))
  }
  
  do.call(rbind.data.frame, c(result, stringsAsFactors = FALSE))
}

#' Extract struct definitions from preprocessed C code
#' @param preprocessed_lines Character vector from tcc_preprocess()
#' @return List of struct definitions (name -> list of fields)
#' @export
tcc_extract_structs <- function(preprocessed_lines) {
  # Remove line markers
  code_lines <- preprocessed_lines[!grepl("^#", preprocessed_lines)]
  code <- paste(code_lines, collapse = " ")
  
  # Remove comments
  code <- gsub("/\\*.*?\\*/", " ", code)
  code <- gsub("//.*?($|;)", ";", code)
  
  result <- list()
  
  # Pattern 1: struct Name { ... };
  pattern1 <- "struct\\s+([a-zA-Z_][a-zA-Z0-9_]*)\\s*\\{([^}]*)\\}\\s*;"
  matches1 <- gregexpr(pattern1, code, perl = TRUE)
  match_data1 <- regmatches(code, matches1)[[1]]
  
  # Pattern 2: typedef struct { ... } TypeName;
  pattern2 <- "typedef\\s+struct\\s*\\{([^}]*)\\}\\s*([a-zA-Z_][a-zA-Z0-9_]*)\\s*;"
  matches2 <- gregexpr(pattern2, code, perl = TRUE)
  match_data2 <- regmatches(code, matches2)[[1]]
  
  # Helper function to parse struct fields
  parse_fields <- function(body) {
    field_pattern <- "([a-zA-Z_][a-zA-Z0-9_*\\s]+)\\s+([a-zA-Z_][a-zA-Z0-9_\\[\\]]*)\\s*;"
    field_matches <- gregexpr(field_pattern, body, perl = TRUE)
    field_data <- regmatches(body, field_matches)[[1]]
    
    fields <- lapply(field_data, function(field) {
      fm <- regexec(field_pattern, field, perl = TRUE)
      fparts <- regmatches(field, fm)[[1]]
      if (length(fparts) >= 3) {
        list(
          type = trimws(fparts[2]),
          name = trimws(fparts[3])
        )
      } else {
        NULL
      }
    })
    
    Filter(Negate(is.null), fields)
  }
  
  # Process pattern 1: struct Name { ... };
  if (length(match_data1) > 0) {
    structs1 <- lapply(match_data1, function(struct_def) {
      m <- regexec(pattern1, struct_def, perl = TRUE)
      parts <- regmatches(struct_def, m)[[1]]
      
      if (length(parts) >= 3) {
        name <- trimws(parts[2])
        body <- trimws(parts[3])
        fields <- parse_fields(body)
        list(name = name, fields = fields)
      } else {
        NULL
      }
    })
    
    structs1 <- Filter(Negate(is.null), structs1)
    for (s in structs1) {
      result[[s$name]] <- s$fields
    }
  }
  
  # Process pattern 2: typedef struct { ... } TypeName;
  if (length(match_data2) > 0) {
    structs2 <- lapply(match_data2, function(struct_def) {
      m <- regexec(pattern2, struct_def, perl = TRUE)
      parts <- regmatches(struct_def, m)[[1]]
      
      if (length(parts) >= 3) {
        body <- trimws(parts[2])
        name <- trimws(parts[3])
        fields <- parse_fields(body)
        list(name = name, fields = fields)
      } else {
        NULL
      }
    })
    
    structs2 <- Filter(Negate(is.null), structs2)
    for (s in structs2) {
      result[[s$name]] <- s$fields
    }
  }
  
  result
}

#' Parse C header file and extract all declarations
#' @param header_file Path to C header file
#' @param includes Additional include directories
#' @return List with components: functions, structs, defines
#' @export
tcc_parse_header <- function(header_file, includes = NULL) {
  # Get preprocessed output
  preprocessed <- tcc_preprocess(header_file, includes = includes)
  
  # Extract components
  list(
    functions = tcc_extract_functions(preprocessed),
    structs = tcc_extract_structs(preprocessed),
    defines = tcc_extract_defines(header_file = header_file, preprocessed_lines = preprocessed)
  )
}
