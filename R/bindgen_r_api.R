#' Generate FFI bindings for R's C API
#'
#' Parses R's header files (Rinternals.h, R.h, Rmath.h) and generates R bindings
#' that allow calling R's internal C functions directly via FFI.
#'
#' @param output_file Path to write the generated R bindings. If NULL, returns
#'   the parsed results without writing.
#' @param headers Character vector of header names to parse. Default includes
#'   "Rinternals.h", "R.h", and "Rmath.h".
#' @param include_path Path to R's include directory. Defaults to `R.home("include")`.
#' @param load_r_lib Logical. If TRUE (default on Windows), attempt to load the R
#'   shared library to ensure symbols are available.
#' @param verbose Logical. If TRUE, print progress messages.
#'
#' @return Invisibly returns a list with parsed results for each header:
#'   \item{structs}{Named list of struct definitions}
#'   \item{functions}{Named list of function signatures}
#'   \item{typedefs}{Named list of typedef mappings}
#'   \item{enums}{Named list of enum definitions}
#'
#' @details
#' On Unix-like systems (Linux, macOS), R's symbols are typically available
#' in the running process. On Windows, the R.dll may need to be explicitly
#' loaded. Set `load_r_lib = TRUE` to handle this automatically.
#'
#' The generated bindings create wrapper functions that use [ffi_function()]
#' to call the underlying C functions. Each wrapper includes roxygen2
#' documentation with parameter types and return values.
#'
#' @section Available Headers:
#' \describe{
#'   \item{Rinternals.h}{Core R internals: SEXP manipulation, memory management,
#'     type checking functions (Rf_isInteger, Rf_length, etc.)}
#'   \item{R.h}{Main R header, includes standard utilities}
#'   \item{Rmath.h}{Statistical distribution functions (dnorm, pnorm, qnorm,
#'     gamma, beta, etc.)}
#' }
#'
#' @examples
#' \dontrun{
#' # Generate bindings to a file
#' bindgen_r_api(output_file = "r_api_bindings.R")
#'
#' # Parse without writing (for inspection)
#' result <- bindgen_r_api(verbose = TRUE)
#' names(result$Rinternals$functions)
#'
#' # Generate only Rmath bindings
#' bindgen_r_api(
#'     output_file = "rmath_bindings.R",
#'     headers = "Rmath.h"
#' )
#'
#' # After sourcing generated bindings:
#' # source("r_api_bindings.R")
#' # r_Rf_dnorm4(0, 0, 1, 0L)  # same as dnorm(0, 0, 1)
#' }
#'
#' @seealso [ffi_parse_header()], [generate_r_bindings()], [ffi_function()]
#' @export
bindgen_r_api <- function(
    output_file = NULL,
    headers = c("Rinternals.h", "R.h", "Rmath.h"),
    include_path = R.home("include"),
    load_r_lib = .is_windows,
    verbose = FALSE) {
    # Ensure R library is loaded (mainly for Windows)
    if (load_r_lib) {
        .ensure_r_lib_loaded(verbose = verbose)
    }

    results <- list()

    for (header in headers) {
        header_path <- file.path(include_path, header)

        if (!file.exists(header_path)) {
            if (verbose) message("Header not found: ", header_path)
            next
        }

        if (verbose) message("Parsing ", header, "...")

        parsed <- tryCatch(
            ffi_parse_header(header_path, includes = include_path),
            error = function(e) {
                if (verbose) message("  Error: ", conditionMessage(e))
                NULL
            }
        )

        if (!is.null(parsed)) {
            # Store using clean name (without .h)
            name <- sub("\\.h$", "", basename(header))
            results[[name]] <- parsed

            if (verbose) {
                message("  Structs: ", length(parsed$structs))
                message("  Functions: ", length(parsed$functions))
                message("  Typedefs: ", length(parsed$typedefs))
                message("  Enums: ", length(parsed$enums))
            }
        }
    }

    # Write combined bindings if output file specified
    if (!is.null(output_file)) {
        if (verbose) message("Writing bindings to ", output_file, "...")

        # Create output directory if needed
        output_dir <- dirname(output_file)
        if (output_dir != "." && !dir.exists(output_dir)) {
            dir.create(output_dir, recursive = TRUE)
        }

        # Combine all results - functions is a data frame, others are lists
        combined <- list(
            file = paste(headers, collapse = ", "), # Required by generate_r_bindings
            structs = list(),
            functions = data.frame(
                name = character(),
                return_type = character(),
                params = character(),
                full_declaration = character(),
                stringsAsFactors = FALSE
            ),
            typedefs = list(),
            enums = list(),
            defines = list()
        )

        for (name in names(results)) {
            parsed <- results[[name]]
            combined$structs <- c(combined$structs, parsed$structs)
            # Bind function data frames
            if (!is.null(parsed$functions) && nrow(parsed$functions) > 0) {
                combined$functions <- rbind(combined$functions, parsed$functions)
            }
            combined$typedefs <- c(combined$typedefs, parsed$typedefs)
            combined$enums <- c(combined$enums, parsed$enums)
            if (!is.null(parsed$defines)) {
                combined$defines <- c(combined$defines, parsed$defines)
            }
        }

        # Generate bindings
        generate_r_bindings(combined, output_file = output_file)

        if (verbose) {
            message("Generated bindings: ", output_file)
            message("  File size: ", file.size(output_file), " bytes")
        }
    }

    invisible(results)
}


#' Ensure R shared library is loaded
#'
#' On Windows, R.dll may not be automatically available for symbol lookup.
#' This function attempts to load it if needed.
#'
#' @param verbose Print messages about loading
#' @return Invisibly returns TRUE if R symbols are available
#' @keywords internal
.ensure_r_lib_loaded <- function(verbose = FALSE) {
    # Test if a common R symbol is already available
    test_sym <- tryCatch(
        ffi_symbol("Rf_isNull"),
        error = function(e) NULL
    )

    if (!is.null(test_sym)) {
        if (verbose) message("R symbols already available")
        return(invisible(TRUE))
    }

    # Try to find and load R library
    r_lib_path <- .find_r_library()

    if (is.null(r_lib_path)) {
        if (verbose) message("Could not locate R shared library")
        return(invisible(FALSE))
    }

    if (verbose) message("Loading R library: ", r_lib_path)

    tryCatch(
        {
            dll_load(r_lib_path)
            invisible(TRUE)
        },
        error = function(e) {
            if (verbose) message("Failed to load R library: ", conditionMessage(e))
            invisible(FALSE)
        }
    )
}


#' Find R shared library path
#'
#' @return Path to R shared library or NULL if not found
#' @keywords internal
.find_r_library <- function() {
    if (.is_windows) {
        # Windows: R.dll is in bin/
        r_home <- R.home()
        candidates <- c(
            file.path(r_home, "bin", "x64", "R.dll"),
            file.path(r_home, "bin", "i386", "R.dll"),
            file.path(r_home, "bin", "R.dll")
        )
    } else if (.is_macos) {
        # macOS: libR.dylib
        r_home <- R.home()
        candidates <- c(
            file.path(r_home, "lib", "libR.dylib"),
            "/Library/Frameworks/R.framework/Resources/lib/libR.dylib"
        )
    } else {
        # Linux/Unix: libR.so
        r_home <- R.home()
        candidates <- c(
            file.path(r_home, "lib", "libR.so"),
            "/usr/lib/R/lib/libR.so",
            "/usr/lib64/R/lib/libR.so"
        )
    }

    for (path in candidates) {
        if (file.exists(path)) {
            return(path)
        }
    }

    NULL
}


#' Get summary of R API bindings
#'
#' Quick summary of what's available in R's C API headers.
#'
#' @param include_path Path to R's include directory
#' @return Data frame with header info
#' @export
#' @examples
#' \dontrun{
#' bindgen_r_api_summary()
#' }
bindgen_r_api_summary <- function(include_path = R.home("include")) {
    headers <- c("Rinternals.h", "R.h", "Rmath.h", "Rdefines.h", "Rembedded.h")

    info <- lapply(headers, function(h) {
        path <- file.path(include_path, h)
        if (file.exists(path)) {
            list(
                header = h,
                exists = TRUE,
                size = file.size(path)
            )
        } else {
            list(header = h, exists = FALSE, size = NA)
        }
    })

    do.call(rbind, lapply(info, as.data.frame))
}
