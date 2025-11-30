#' @name dll_load
#' @title Load a shared library/DLL
#' @param filename Path to the shared library
#' @param now Whether to resolve all symbols immediately (default TRUE)
#' @param local Keep symbols local to avoid namespace pollution (default TRUE)
#' @param verbose Print loading information (default FALSE)
#' @return Library handle (character string of loaded library path)
#' @rdname dynamic_library_management
#' @export
dll_load <- function(filename, now = TRUE, local = TRUE, verbose = FALSE) {
  if (!file.exists(filename)) {
    stop("Library file not found: ", filename)
  }

  # Use R's built-in dyn.load
  result <- tryCatch(
    {
      dyn.load(filename, now = now, local = local, verbose = verbose)
      filename # Return the filename as handle
    },
    error = function(e) {
      stop("Failed to load library '", filename, "': ", e$message)
    }
  )

  if (verbose) {
    message("Successfully loaded library:", filename, "\n")
  }

  result
}

#' Unload a shared library/DLL
#' @name dll_unload
#' @param handle Library handle (path) returned by dll_load()
#' @param verbose Print unloading information (default FALSE)
#' @return dyn.unload result invisibly
#' @rdname dynamic_library_management
#' @export
dll_unload <- function(handle, verbose = FALSE) {
  if (!is.character(handle)) {
    stop("Handle must be a character string (library path)")
  }

  result <- tryCatch(
    {
      dyn.unload(handle)
      if (verbose) {
        message("Successfully unloaded library:", handle, "\n")
      }
      TRUE
    },
    error = function(e) {
      warning("Failed to unload library '", handle, "': ", e$message)
      FALSE
    }
  )

  invisible(result)
}

#' Get symbol information from a loaded library
#'
#' @param symbol_name Name of the symbol to find
#' @param package Package name where symbol is registered (optional)
#' @return Symbol information including address as external pointer
#' @rdname dynamic_library_management
#' @export
dll_symbol <- function(symbol_name, package = NULL) {
  if (!is.character(symbol_name) || length(symbol_name) != 1) {
    stop("Symbol name must be a single character string")
  }

  # Try to get symbol info
  symbol_info <- tryCatch(
    {
      if (is.null(package)) {
        getNativeSymbolInfo(symbol_name)
      } else {
        getNativeSymbolInfo(symbol_name, package)
      }
    },
    error = function(e) {
      stop("Symbol '", symbol_name, "' not found: ", e$message)
    }
  )

  if (is.null(symbol_info$address)) {
    stop("Symbol '", symbol_name, "' has NULL address")
  }

  symbol_info
}

#' Check if a symbol is loaded
#'
#' @param symbol_name Name of the symbol to check
#' @param package Package name (optional)
#' @return TRUE if symbol is loaded, FALSE otherwise
#' @export
dll_is_loaded <- function(symbol_name, package = NULL) {
  if (!is.character(symbol_name) || length(symbol_name) != 1) {
    stop("Symbol name must be a single character string")
  }

  tryCatch(
    {
      if (is.null(package)) {
        is.loaded(symbol_name)
      } else {
        is.loaded(symbol_name, PACKAGE = package)
      }
    },
    error = function(e) {
      FALSE
    }
  )
}

#' Create FFI function from dynamically loaded function
#'
#' This creates an FFI function wrapper for dynamically loaded native C functions.
#' Uses direct address access like Rffi for maximum compatibility.
#'
#' @param symbol_name Name of the symbol
#' @param return_type Return type specifimessageion
#' @param ... Argument type specifimessageions
#' @param package Package name (optional)
#' @return FFI function object that can be called directly
#' @export
dll_ffi_symbol <- function(symbol_name, return_type, ..., package = NULL) {
  # Get symbol info using R's facilities (like Rffi does)
  symbol_info <- dll_symbol(symbol_name, package)

  # Create CIF for the function signature
  cif <- ffi_cif(return_type, ...)

  # Create symbol object from address
  symbol <- ffi_symbol_from_address(symbol_info$address, symbol_name)

  # Return a closure that calls the function
  function(...) {
    ffi_call(cif, symbol, ...)
  }
}

#' List loaded libraries
#'
#' @return Character vector of loaded library paths
#' @export
dll_list_loaded <- function() {
  # Get currently loaded DLLs
  loaded <- getLoadedDLLs()
  vapply(loaded, function(x) x[["path"]], character(1))
}

#' Get information about a loaded library
#'
#' @param handle Library handle (path)
#' @return List with library information
#' @export
dll_info <- function(handle) {
  if (!is.character(handle)) {
    stop("Handle must be a character string (library path)")
  }

  loaded <- getLoadedDLLs()

  # Find the library by path
  lib_info <- NULL
  for (dll in loaded) {
    if (dll[["path"]] == handle) {
      lib_info <- dll
      break
    }
  }

  if (is.null(lib_info)) {
    message("Library not found in loaded DLLs: ", handle)
    message("Currently loaded DLLs:")
    for (dll in loaded) {
      message(" - ", dll[["path"]])
    }
    stop("Library not found in loaded DLLs: ", handle)
  }

  list(
    name = lib_info[["name"]],
    path = lib_info[["path"]],
    dynamicLookup = lib_info[["DLLInfo"]][["dynamicLookup"]],
    handle = lib_info[["DLLInfo"]][["handle"]]
  )
}

#' Compile and load C code into a DLL
#'
#' This function compiles C code using R's configured compiler and loads it.
#' Uses the same compiler configuration that R was built with.
#'
#' @param code Character vector of C code to compile
#' @param name Base name for the compiled library (default "temp_dll")
#' @param includes Additional include directories
#' @param libs Additional libraries to link
#' @param verbose Print compilation output (default FALSE)
#' @param cflags Additional compiler flags (e.g., "-O2", "-O3")
#' @param compilation_directory Directory to use for compilation (default temp dir)
#' @return Library handle that can be used with dll_* functions
#' @rdname dynamic_library_management
#' @export
dll_compile_and_load <- function(
    code,
    name = "temp_dll",
    includes = NULL,
    libs = NULL,
    verbose = FALSE,
    cflags = NULL,
    compilation_directory = tempfile("dll_compile_")) {
  # Create temporary directory and files
  if (!dir.exists(compilation_directory)) {
    dir.create(compilation_directory, recursive = TRUE)
  }
  # Change to temp directory for compilation
  old_wd <- getwd()
  setwd(compilation_directory)
  on.exit(setwd(old_wd), add = TRUE)
  c_file <- file.path(compilation_directory, paste0(name, ".c"))
  so_file <- file.path(compilation_directory, paste0(name, .Platform$dynlib.ext))
  # Write C code to file
  writeLines(code, basename(c_file))
  c_file <- normalizePath(c_file, winslash = "/", mustWork = TRUE)
  so_file <- normalizePath(so_file, winslash = "/", mustWork = FALSE)
  # Create Makevars file for compilation options
  makevars_content <- character(0)

  if (!is.null(includes)) {
    include_flags <- paste(paste0("-I", includes), collapse = " ")
    makevars_content <- c(
      makevars_content,
      paste("PKG_CPPFLAGS =", include_flags)
    )
  }

  if (!is.null(libs)) {
    lib_flags <- paste(paste0("-l", libs), collapse = " ")
    makevars_content <- c(makevars_content, paste("PKG_LIBS =", lib_flags))
  }

  if (!is.null(cflags)) {
    makevars_content <- c(makevars_content, paste("PKG_CFLAGS =", cflags))
  }

  # Write Makevars file if we have options
  if (length(makevars_content) > 0) {
    makevars_file <- file.path(compilation_directory, "Makevars")
    writeLines(makevars_content, makevars_file)

    # Set R_MAKEVARS_USER to point to our Makevars
    old_makevars <- Sys.getenv("R_MAKEVARS_USER", unset = NA)
    Sys.setenv(R_MAKEVARS_USER = makevars_file)
    on.exit(
      {
        if (is.na(old_makevars)) {
          Sys.unsetenv("R_MAKEVARS_USER")
        } else {
          Sys.setenv(R_MAKEVARS_USER = old_makevars)
        }
      },
      add = TRUE
    )
  }

  # Build R CMD SHLIB command
  r_cmd <- file.path(R.home("bin/R"))
  cmd_args <- c("CMD", "SHLIB", "-o", basename(so_file), basename(c_file))

  # Compile using R CMD SHLIB
  if (verbose) {
    tryCatch(
      {
        output <- system2(r_cmd, cmd_args, stdout = "", stderr = "")
      },
      error = function(e) {
        stop("Compilation failed: ", e$message)
      }
    )
  } else {
    tryCatch(
      {
        output <- system2(r_cmd, cmd_args, stdout = TRUE, stderr = TRUE)
      },
      error = function(e) {
        stop("Compilation failed: ", e$message)
      }
    )
  }

  # Get status
  status <- attr(output, "status")
  if (is.null(status)) {
    status <- 0
  }

  # Handle compilation output
  if (verbose || status != 0) {
    if (length(output) > 0) {
      message("Compilation output:")
      message(paste(output, collapse = "\n"))
    }
  }

  if (status != 0) {
    stop("Compilation failed with status ", status)
  }

  if (!file.exists(so_file)) {
    stop("Shared library not created: ", so_file)
  }

  # Load the library
  dll_load(so_file, verbose = verbose)
}


# ---- OS detection ----
.is_windows <- tolower(Sys.info()[["sysname"]]) == "windows"
.is_macos <- tolower(Sys.info()[["sysname"]]) == "darwin"
.is_linux <- tolower(Sys.info()[["sysname"]]) == "linux"

.system_paths <- function() {
  if (.is_windows) {
    return(c(
      Sys.getenv("SystemRoot"),
      file.path(Sys.getenv("SystemRoot"), "System32")
    ))
  } else if (.is_macos) {
    return(c(
      "/lib",
      "/lib64",
      "/usr/lib",
      "/usr/lib64",
      "/usr/lib/x86_64-linux-gnu",
      "/usr/local/lib",
      "/usr/local/lib32", # wsl
      "/usr/local/lib64"
    ))
  } else if (.is_linux) {
    return(c(
      "/lib",
      "/lib64",
      "/usr/lib",
      "/usr/lib64",
      "/usr/lib/x86_64-linux-gnu",
      "/usr/local/lib",
      "/usr/local/lib32", # wsl
      "/usr/local/lib64"
    ))
  } else {
    return(character(0))
  }
}

#' @name dll_load_system
#' @title Load system library
#' Load a system shared library/DLL by searching common system paths.
#' @param lib_name Name of system library (e.g., libc.so.6, libm.dylib, kernel32.dll)
#' @param verbose Print loading information (default FALSE)
#' @return Library handle or NULL if not found
#' @rdname dynamic_library_management
#' @export
dll_load_system <- function(lib_name, verbose = FALSE) {
  # Common system library paths
  system_paths <- .system_paths()
  for (path in system_paths) {
    full_path <- file.path(path, lib_name)
    if (file.exists(full_path)) {
      message("Loading system library from: ", full_path)

      tryCatch(
        {
          handle <- dll_load(full_path, verbose = verbose)
          return(handle)
        },
        error = function(e) {
          warning("Failed to load from ", full_path, ": ", e$message)
        }
      )
      return()
    }
  }
  warning("System library not found: ", lib_name)
  NULL
}
