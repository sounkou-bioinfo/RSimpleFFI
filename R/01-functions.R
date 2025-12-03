#####################################
#
# FFI CIF
#
######################################

# Helper to get the appropriate ffi_type ref for CIF preparation
# For structs/unions with packed layout changes, by-value passing is not supported
# (libffi depends on field offsets for calling conventions which don't match packed layout)
get_cif_type_ref <- function(type) {
  # Check for packed structs/unions with layout changes - these can't be passed by value
  if (S7::S7_inherits(type, StructType) || S7::S7_inherits(type, UnionType)) {
    if (isTRUE(type@has_packed_change)) {
      stop(sprintf(
        "Packed %s '%s' cannot be passed by value to a C function ",
        if (S7::S7_inherits(type, StructType)) "struct" else "union",
        type@name %||% "(anonymous)"
      ), "(libffi limitation). Use a pointer instead.", call. = FALSE)
    }
  }
  type@ref
}

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

  # Get refs, using byval type for packed structs
  return_ref <- get_cif_type_ref(return_type)
  arg_refs <- if (length(arg_types) > 0) {
    lapply(arg_types, get_cif_type_ref)
  } else {
    list()
  }

  cif_ref <- .Call("R_prep_ffi_cif", return_ref, arg_refs)

  CIF(
    return_type = return_type,
    arg_types = arg_types,
    ref = cif_ref
  )
}

#' Prepare FFI call interface for variadic functions
#'
#' Creates a CIF for calling C functions with variable arguments (varargs).
#' Unlike regular CIFs, variadic CIFs must specify the types of ALL arguments
#' for each specific call, including the variadic ones.
#'
#' @param return_type FFIType for return value
#' @param nfixedargs Number of fixed arguments (before the ...)
#' @param ... FFIType objects for ALL arguments (fixed + variadic)
#' @return CIF object
#'
#' @details
#' Due to C calling conventions, variadic arguments undergo "default argument
#' promotions": float becomes double, and small integers (char, short) become int.
#' You must use ffi_int() or ffi_double() for variadic arguments, not smaller types.
#'
#' @examples
#' \dontrun{
#' # Call a varargs function: test_varargs_sum(int nargs, ...)
#' # First argument (nargs) is fixed, rest are variadic integers
#' sym <- ffi_symbol("test_varargs_sum")
#'
#' # Call with 3 variadic int arguments
#' cif <- ffi_cif_var(ffi_double(),
#'   nfixedargs = 1L,
#'   ffi_int(), ffi_int(), ffi_int(), ffi_int()
#' )
#' result <- ffi_call(cif, sym, 3L, 10L, 20L, 30L) # returns 60
#' }
#' @export
ffi_cif_var <- function(return_type, nfixedargs, ...) {
  if (!S7::S7_inherits(return_type, FFIType)) {
    stop("return_type must be an FFIType object")
  }

  if (!is.numeric(nfixedargs) || length(nfixedargs) != 1 || nfixedargs < 0) {
    stop("nfixedargs must be a non-negative integer")
  }

  arg_types <- list(...)

  if (length(arg_types) > 0 &&
    !all(sapply(arg_types, function(x) S7::S7_inherits(x, FFIType)))) {
    stop("All argument types must be FFIType objects")
  }

  if (as.integer(nfixedargs) > length(arg_types)) {
    stop("nfixedargs cannot exceed total number of argument types")
  }

  # Get refs, using byval type for packed structs
  return_ref <- get_cif_type_ref(return_type)
  arg_refs <- if (length(arg_types) > 0) {
    lapply(arg_types, get_cif_type_ref)
  } else {
    list()
  }

  cif_ref <- .Call(
    "R_prep_ffi_cif_var", return_ref, arg_refs,
    as.integer(nfixedargs)
  )

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
#'
#' @description
#' Call a C function through the FFI interface.
#'
#' @details
#' The method implementations accept an additional `na_check` argument (logical,
#' default TRUE). When TRUE, the function checks for NA values in arguments and
#' errors if found. Set to FALSE to skip NA checking for better performance
#' (at your own risk).
#'
#' ## Error Handling Limitations
#'
#' **Important:** libffi provides no error handling for the actual C function call.
#' If the called C function crashes (segmentation fault, illegal instruction,
#' abort, etc.), R itself will crash. This is a fundamental limitation of FFI
#' - there is no portable way to catch such errors in C code.
#'
#' Before making FFI calls, ensure:
#' \itemize{
#'   \item The function pointer is valid (not NULL, points to executable code)
#'   \item All pointer arguments are valid (use \code{\link{ffi_is_null}} to check)
#'   \item Array/buffer sizes are correct - buffer overruns cause undefined behavior
#'   \item The CIF signature exactly matches the C function's signature
#'   \item Struct layouts match between R types and C (check alignment/padding)
#' }
#'
#' For debugging crashes:
#' \itemize{
#'   \item Run R under a debugger: \code{R -d gdb}
#'   \item Enable core dumps: \code{ulimit -c unlimited}
#'   \item Use address sanitizers when building the library being called
#' }
#'
#' @param cif CIF object defining the call interface
#' @param symbol NativeSymbol or character name of function
#' @param ... Arguments to pass to the function (including `na_check`)
#' @return The return value from the C function, converted to an R type
#' @seealso \code{\link{ffi_is_null}} for checking pointer validity
#' @export
ffi_call <- S7::new_generic("ffi_call", c("cif", "symbol"))

# NativeSymbol variant
S7::method(ffi_call, list(CIF, NativeSymbol)) <- function(
    cif,
    symbol,
    ...,
    na_check = TRUE) {
  args <- list(...)
  expected_args <- length(cif@arg_types)

  if (length(args) != expected_args) {
    stop("Expected ", expected_args, " arguments, got ", length(args))
  }

  .Call("R_ffi_call", cif@ref, symbol@address, args, as.logical(na_check))
}

# symbol as character name variant
S7::method(ffi_call, list(CIF, S7::class_character)) <- function(
    cif,
    symbol,
    ...,
    na_check = TRUE) {
  sym <- ffi_symbol(symbol)
  ffi_call(cif, sym, ..., na_check = na_check)
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
#' @param na_check Logical; if TRUE (default), check for NA values and error if found.
#'   Set to FALSE to skip NA checking for better performance (at your own risk).
#' @export
ffi_function <- function(name, return_type, ..., library = NULL, na_check = TRUE) {
  # Create CIF and symbol
  cif <- ffi_cif(return_type, ...)
  symbol <- ffi_symbol(name, library)
  # Capture na_check setting
  check_na <- as.logical(na_check)

  # Return a closure that calls the function
  function(...) {
    ffi_call(cif, symbol, ..., na_check = check_na)
  }
}


#####################################
#
# FFI Closure API
#
# Wrap R functions as C callbacks
######################################

#' Check if closures are supported on this platform
#'
#' Not all platforms support FFI closures. Use this function to check
#' before attempting to create closures.
#'
#' @return Logical; TRUE if closures are supported
#' @export
ffi_closures_supported <- function() {
  .Call("R_ffi_closures_supported")
}

#' Create an FFI closure from an R function
#'
#' Wraps an R function so it can be used as a callback from C code.
#' The closure has a CIF that describes its signature (return type and
#' argument types). When C code calls through the closure's function
#' pointer, the R function is invoked with converted arguments.
#'
#' @param r_function An R function to wrap as a callback
#' @param return_type FFIType for return value
#' @param ... FFIType objects for arguments
#' @return An FFIClosure object
#'
#' @details
#' The R function must accept the same number of arguments as specified

#' in the type signature. Arguments are converted from C types to R types
#' before calling, and the return value is converted back to C.
#'
#' Important: You must keep a reference to the FFIClosure object for as
#' long as C code might call through it. If the closure is garbage collected,
#' calling through its function pointer will crash.
#'
#' @examples
#' \dontrun{
#' # Create a comparison function for qsort
#' cmp_fn <- function(a, b) {
#'   as.integer(a - b)
#' }
#'
#' # Wrap it as a C callback: int (*)(int*, int*)
#' cmp_closure <- ffi_closure(
#'   cmp_fn,
#'   ffi_int(), # return type
#'   ffi_pointer(), ffi_pointer() # argument types (pointers to int)
#' )
#'
#' # Get the function pointer to pass to C
#' cmp_ptr <- ffi_closure_pointer(cmp_closure)
#' }
#'
#' @seealso [ffi_closure_pointer()] to get the callable function pointer
#' @export
ffi_closure <- function(r_function, return_type, ...) {
  if (!is.function(r_function)) {
    stop("r_function must be a function")
  }

  if (!ffi_closures_supported()) {
    stop("FFI closures are not supported on this platform")
  }

  # Create CIF for the callback signature
  cif <- ffi_cif(return_type, ...)

  # Create the closure
  closure_ref <- .Call("R_create_closure", r_function, cif@ref)

  # Get the executable function pointer
  func_ptr <- .Call("R_get_closure_pointer", closure_ref)

  FFIClosure(
    r_function = r_function,
    cif = cif,
    ref = closure_ref,
    func_ptr = func_ptr
  )
}

#' Get the function pointer for an FFI closure
#'
#' Returns the executable function pointer that can be passed to C functions
#' expecting a callback.
#'
#' @param closure An FFIClosure object
#' @return External pointer to the callable function
#'
#' @details
#' The returned pointer can be passed to C functions via `ffi_call()`.
#' It will invoke the R function when called.
#'
#' @seealso [ffi_closure()] to create closures
#' @export
ffi_closure_pointer <- function(closure) {
  if (!S7::S7_inherits(closure, FFIClosure)) {
    stop("closure must be an FFIClosure object")
  }
  closure@func_ptr
}
