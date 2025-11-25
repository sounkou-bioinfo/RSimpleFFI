# RSimpleFFI Utilities

#' @import S7  
NULL

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

# Pretty printing methods
S7::method(format, FFIType) <- function(x, ...) {
  paste0("FFIType(", x@name, ", size=", x@size, ")")
}

S7::method(format, StructType) <- function(x, ...) {
  fields_str <- paste(x@fields, collapse = ", ")
  paste0("StructType(fields=[", fields_str, "], size=", x@size, ")")
}

S7::method(format, CIF) <- function(x, ...) {
  if (length(x@arg_types) > 0) {
    arg_names <- sapply(x@arg_types, function(t) t@name)
    args_str <- paste(arg_names, collapse = ", ")
  } else {
    args_str <- ""
  }
  paste0("CIF(", x@return_type@name, " <- (", args_str, "))")
}

S7::method(format, NativeSymbol) <- function(x, ...) {
  lib_part <- if (x@library != "") paste0(" from ", x@library) else ""
  paste0("NativeSymbol(", x@name, lib_part, ")")
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
      message("  ", x@fields[i], ": ", format(x@field_types[[i]]), "\n", sep = "")
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