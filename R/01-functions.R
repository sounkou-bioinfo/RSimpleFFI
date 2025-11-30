#####################################
#
# FFI CIF
#
######################################

#' @name ffi_cif
#' @title Prepare FFI call interface
#' Prepare FFI call interface
#' @param return_type FFIType for return value
#' @param ... FFIType objects for arguments
#' @export
ffi_cif <- function(return_type, ...) {
  if (!S7::S7_inherits(return_type, FFIType)) {
    stop("return_type must be an FFIType object")
  }

  arg_types <- list(...)

  # Validate all arguments are FFIType objects
  if (
    length(arg_types) > 0 &&
      !all(sapply(arg_types, function(x) S7::S7_inherits(x, FFIType)))
  ) {
    stop("All argument types must be FFIType objects")
  }

  arg_refs <- if (length(arg_types) > 0) {
    lapply(arg_types, function(x) x@ref)
  } else {
    list()
  }

  cif_ref <- .Call("R_prep_ffi_cif", return_type@ref, arg_refs)

  CIF(
    return_type = return_type,
    arg_types = arg_types,
    ref = cif_ref
  )
}


#####################################
#
# FFI Symbols
#
######################################

#' Get native symbol reference
#' @param name Character name of the symbol
#' @param library Character name of library (optional)
#' @export
ffi_symbol <- function(name, library = NULL) {
  if (!is.character(name) || length(name) != 1) {
    stop("name must be a single character string")
  }

  if (is.null(library)) {
    # Try to find in loaded DLLs, including RSimpleFFI itself
    info <- tryCatch(
      {
        getNativeSymbolInfo(name)
      },
      error = function(e) {
        # Try in RSimpleFFI package specifically
        tryCatch(
          {
            getNativeSymbolInfo(name, "RSimpleFFI")
          },
          error = function(e2) {
            NULL
          }
        )
      }
    )

    if (is.null(info)) {
      stop(
        "Symbol not found: ",
        name,
        ". Make sure the library containing this symbol is loaded."
      )
    }

    lib_name <- if (!is.null(info$package)) info$package else ""
  } else {
    info <- getNativeSymbolInfo(name, library)
    lib_name <- library
  }

  NativeSymbol(
    name = name,
    address = info$address,
    library = lib_name
  )
}

#' Create native symbol from direct address
#' @param address External pointer to the symbol address
#' @param name Character name of the symbol (for reference)
#' @export
ffi_symbol_from_address <- function(address, name = "anonymous") {
  if (is.null(address)) {
    stop("Address cannot be NULL")
  }

  NativeSymbol(
    name = name,
    address = address,
    library = ""
  )
}


#####################################
#
# FFI CALLS
#
######################################

#' Make FFI function call
#' @param cif CIF object defining the call interface
#' @param symbol NativeSymbol or character name of function
#' @param ... Arguments to pass to the function
#' @export
ffi_call <- S7::new_generic("ffi_call", c("cif", "symbol"))

# NativeSymbol variant
#' @export
S7::method(ffi_call, list(CIF, NativeSymbol)) <- function(cif, symbol, ...) {
  args <- list(...)
  expected_args <- length(cif@arg_types)

  if (length(args) != expected_args) {
    stop("Expected ", expected_args, " arguments, got ", length(args))
  }

  .Call("R_ffi_call", cif@ref, symbol@address, args)
}

# symbol as character name variant
#' @export
S7::method(ffi_call, list(CIF, S7::class_character)) <- function(
  cif,
  symbol,
  ...
) {
  sym <- ffi_symbol(symbol)
  ffi_call(cif, sym, ...)
}


#####################################
#
# FFI Function Wrappers
#
######################################

#' Create a reusable FFI function wrapper
#' @param name Character name of the function
#' @param return_type FFIType for return value
#' @param ... FFIType objects for arguments
#' @param library Character name of library (optional)
#' @export
ffi_function <- function(name, return_type, ..., library = NULL) {
  # Create CIF and symbol
  cif <- ffi_cif(return_type, ...)
  symbol <- ffi_symbol(name, library)

  # Return a closure that calls the function
  function(...) {
    ffi_call(cif, symbol, ...)
  }
}
