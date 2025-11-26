if (getRversion() >= "2.15.1") {
  utils::globalVariables("ArrayType")
}

#' Fill a typed buffer from an R vector (int or double)
#' @param ptr External pointer to buffer
#' @param values Integer or double vector
#' @param type FFIType object
#' @export
ffi_fill_typed_buffer <- function(ptr, values, type) {
  .Call("R_fill_typed_buffer", ptr, values, type@ref)
}


#' Allocate a raw memory buffer (external pointer, auto-finalized)
#'
#' Allocates a buffer of the given size (in bytes) and returns an external pointer.
#' The memory is automatically freed when the pointer is garbage collected.
#' @param size Number of bytes to allocate
#' @return External pointer to buffer
#' @export
ffi_alloc_buffer <- function(size) {
  .Call("R_alloc_buffer", as.integer(size))
}


#' Check if pointer is NULL
#' @param ptr External pointer to check
#' @export
is_null_pointer <- function(ptr) {
  .Call("R_is_null_pointer", ptr)
}

#' Copy array from native memory
#' @param ptr External pointer to array
#' @param length Integer length of array
#' @param element_type FFIType of array elements
#' @export
ffi_copy_array <- function(ptr, length, element_type) {
  if (!S7::S7_inherits(element_type, FFIType)) {
    stop("element_type must be an FFIType object")
  }
  .Call("R_copy_array", ptr, as.integer(length), element_type@ref)
}

#' Copy array from native memory (ArrayType version)
#' @param ptr External pointer to array
#' @param array_type ArrayType object
#' @export
ffi_copy_array_type <- function(ptr, array_type) {
  if (!S7::S7_inherits(array_type, ArrayType)) {
    stop("array_type must be an ArrayType object")
  }
  .Call(
    "R_copy_array",
    ptr,
    as.integer(array_type@length),
    array_type@element_type@ref
  )
}

# Pretty printing methods
#' @export
S7::method(format, FFIType) <- function(x, ...) {
  paste0("FFIType(", x@name, ", size=", x@size, ")")
}

#' @export
S7::method(format, StructType) <- function(x, ...) {
  fields_str <- paste(x@fields, collapse = ", ")
  paste0("StructType(fields=[", fields_str, "], size=", x@size, ")")
}

#' @export
S7::method(format, CIF) <- function(x, ...) {
  if (length(x@arg_types) > 0) {
    arg_names <- sapply(x@arg_types, function(t) t@name)
    args_str <- paste(arg_names, collapse = ", ")
  } else {
    args_str <- ""
  }
  paste0("CIF(", x@return_type@name, " <- (", args_str, "))")
}

#' @export
S7::method(format, NativeSymbol) <- function(x, ...) {
  lib_part <- if (x@library != "") paste0(" from ", x@library) else ""
  paste0("NativeSymbol(", x@name, lib_part, ")")
}

# Print methods for proper output capture in tests
#' @export
S7::method(print, FFIType) <- function(x, ...) {
  cat(format(x), "\n", sep = "")
  invisible(x)
}

#' @export
S7::method(print, StructType) <- function(x, ...) {
  cat(format(x), "\n", sep = "")
  cat("Fields:", "\n", sep = "")
  for (i in seq_along(x@fields)) {
    cat("  ", x@fields[i], ": ", x@field_types[[i]]@name, "\n", sep = "")
  }
  invisible(x)
}

#' @export
S7::method(print, CIF) <- function(x, ...) {
  cat(format(x), "\n", sep = "")
  invisible(x)
}

#' @export
S7::method(print, NativeSymbol) <- function(x, ...) {
  cat(format(x), "\n", sep = "")
  invisible(x)
}

#' Convert pointer to string safely
#'
#' Explicitly converts an external pointer to a character string.
#' Use this instead of relying on automatic conversion heuristics.
#'
#' @param ptr External pointer that points to a null-terminated string
#' @return Character vector of length 1, or NULL if pointer is NULL
#' @export
pointer_to_string <- function(ptr) {
  if(!is.null(ptr) && !inherits(ptr, "externalptr")) {
    stop("ptr must be an external pointer or NULL")
  }
  if (is.null(ptr)) {
    return(NULL)
  }
  .Call("R_pointer_to_string", ptr)
}

#' Create typed external pointer
#'
#' Creates an external pointer with a specific type tag for better type safety,
#' similar to Rffi's approach.
#'
#' @param ptr External pointer
#' @param type_name Character string describing the pointer type
#' @return External pointer with type tag
#' @export
make_typed_pointer <- function(ptr, type_name) {
  if (!is.character(type_name) || length(type_name) != 1) {
    stop("type_name must be a single character string")
  }
  .Call("R_make_typed_pointer", ptr, type_name)
}

#' Get pointer type tag
#'
#' Retrieves the type tag from an external pointer.
#'
#' @param ptr External pointer
#' @return Character string with the type name
#' @export
get_pointer_type <- function(ptr) {
  .Call("R_get_pointer_type", ptr)
}

# Print methods (using format)
S7::method(print, FFIType) <- function(x, ...) {
  message(format(x), "\n")
  invisible(x)
}

S7::method(print, StructType) <- function(x, ...) {
  message(format(x), "\n")
  if (length(x@fields) > 0) {
    message("Fields:\n")
    for (i in seq_along(x@fields)) {
      message(
        "  ",
        x@fields[i],
        ": ",
        format(x@field_types[[i]]),
        "\n",
        sep = ""
      )
    }
  }
  invisible(x)
}

S7::method(print, CIF) <- function(x, ...) {
  message(format(x), "\n")
  invisible(x)
}

S7::method(print, NativeSymbol) <- function(x, ...) {
  message(format(x), "\n")
  invisible(x)
}

#' Get information about loaded native libraries
#' @export
ffi_loaded_libs <- function() {
  libs <- getLoadedDLLs()
  data.frame(
    name = names(libs),
    path = sapply(libs, function(x) x[["path"]]),
    stringsAsFactors = FALSE
  )
}

#' Convert pointer to string safely
#' @param ptr External pointer to convert
#' @return Character string or NULL if pointer is NULL
#' @export
pointer_to_string_safe <- function(ptr) {
  .Call("R_pointer_to_string", ptr)
}