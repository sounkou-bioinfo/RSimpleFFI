# Helper Functions for Common FFI Patterns

#' Create and initialize a struct from R list
#'
#' @param struct_type StructType object
#' @param values Named list of field values
#' @return External pointer to allocated and initialized struct
#' @export
#' @examples
#' \dontrun{
#' Point <- ffi_struct(x = ffi_int(), y = ffi_int())
#' pt <- ffi_struct_from_list(Point, list(x = 10L, y = 20L))
#' }
ffi_struct_from_list <- function(struct_type, values) {
  if (!S7::S7_inherits(struct_type, StructType)) {
    stop("struct_type must be a StructType")
  }

  if (!is.list(values)) {
    stop("values must be a named list")
  }

  # Allocate struct
  ptr <- ffi_alloc(struct_type)

  # Set fields
  for (field_name in names(values)) {
    if (!field_name %in% struct_type@fields) {
      warning("Unknown field '", field_name, "' - skipping")
      next
    }
    ffi_set_field(ptr, field_name, values[[field_name]], struct_type)
  }

  ptr
}

#' Convert struct to R list
#'
#' @param ptr External pointer to struct
#' @param struct_type StructType object
#' @return Named list of field values
#' @export
#' @examples
#' \dontrun{
#' Point <- ffi_struct(x = ffi_int(), y = ffi_int())
#' pt <- ffi_alloc(Point)
#' ffi_set_field(pt, "x", 42L, Point)
#' ffi_set_field(pt, "y", 100L, Point)
#' as.list(pt, Point) # list(x = 42L, y = 100L)
#' }
ffi_struct_to_list <- function(ptr, struct_type) {
  if (!S7::S7_inherits(struct_type, StructType)) {
    stop("struct_type must be a StructType")
  }

  result <- list()
  for (field_name in struct_type@fields) {
    result[[field_name]] <- ffi_get_field(ptr, field_name, struct_type)
  }

  result
}

#' Allocate array of structs from R list
#'
#' @param struct_type StructType object
#' @param values List of named lists, one per struct
#' @return External pointer to allocated struct array
#' @export
#' @examples
#' \dontrun{
#' Point <- ffi_struct(x = ffi_int(), y = ffi_int())
#' points <- ffi_struct_array_from_list(Point, list(
#'   list(x = 0L, y = 0L),
#'   list(x = 10L, y = 20L),
#'   list(x = 30L, y = 40L)
#' ))
#' }
ffi_struct_array_from_list <- function(struct_type, values) {
  if (!S7::S7_inherits(struct_type, StructType)) {
    stop("struct_type must be a StructType")
  }

  if (!is.list(values)) {
    stop("values must be a list of lists")
  }

  n <- length(values)
  if (n == 0) {
    stop("values must contain at least one element")
  }

  # Allocate array
  ptr <- ffi_alloc(struct_type, n)

  # Set each struct
  for (i in seq_along(values)) {
    elem_ptr <- ffi_get_element(ptr, i, struct_type)
    for (field_name in names(values[[i]])) {
      if (field_name %in% struct_type@fields) {
        ffi_set_field(elem_ptr, field_name, values[[i]][[field_name]], struct_type)
      }
    }
  }

  ptr
}

#' Check if external pointer is NULL
#'
#' @param ptr External pointer
#' @return Logical
#' @export
ffi_is_null <- function(ptr) {
  .Call("R_is_null_pointer", ptr)
}

#' Create a NULL pointer
#'
#' @return External pointer to NULL
#' @export
ffi_null_pointer <- function() {
  # Return an external pointer to NULL
  .Call("R_alloc_buffer", 0L) # This returns NULL pointer
}

#' Pretty print struct contents
#'
#' @param ptr External pointer to struct
#' @param struct_type StructType object
#' @export
ffi_print_struct <- function(ptr, struct_type) {
  if (!S7::S7_inherits(struct_type, StructType)) {
    stop("struct_type must be a StructType")
  }

  cat("Struct (", struct_type@name, "):\n", sep = "")

  for (i in seq_along(struct_type@fields)) {
    field_name <- struct_type@fields[i]
    field_type <- struct_type@field_types[[i]]
    value <- ffi_get_field(ptr, field_name, struct_type)

    cat("  ", field_name, " (", field_type@name, "): ", sep = "")

    # Format value based on type
    if (is.null(value)) {
      cat("NULL\n")
    } else if (length(value) == 1) {
      cat(value, "\n")
    } else {
      cat("[", paste(utils::head(value, 5), collapse = ", "),
        if (length(value) > 5) "..." else "", "]\n",
        sep = ""
      )
    }
  }

  invisible(ptr)
}

#' Convert enum name to integer value
#'
#' Look up the integer value for a named enum constant.
#'
#' @param enum_type EnumType object
#' @param name Character name of enum constant
#' @return Integer value
#' @export
#' @examples
#' \dontrun{
#' Color <- ffi_enum(RED = 0L, GREEN = 1L, BLUE = 2L)
#' ffi_enum_to_int(Color, "GREEN") # 1L
#' }
ffi_enum_to_int <- function(enum_type, name) {
  if (!S7::S7_inherits(enum_type, EnumType)) {
    stop("enum_type must be an EnumType object")
  }
  if (!is.character(name) || length(name) != 1) {
    stop("name must be a single character string")
  }

  value <- enum_type@values[name]
  if (is.na(value)) {
    stop(
      "No such enum constant '", name, "'. Available: ",
      paste(names(enum_type@values), collapse = ", ")
    )
  }

  as.integer(value)
}

#' Convert integer value to enum name
#'
#' Look up the enum constant name for an integer value.
#'
#' @param enum_type EnumType object
#' @param value Integer value
#' @return Character name of enum constant (or NA if not found)
#' @export
#' @examples
#' \dontrun{
#' Color <- ffi_enum(RED = 0L, GREEN = 1L, BLUE = 2L)
#' ffi_int_to_enum(Color, 1L) # "GREEN"
#' }
ffi_int_to_enum <- function(enum_type, value) {
  if (!S7::S7_inherits(enum_type, EnumType)) {
    stop("enum_type must be an EnumType object")
  }
  if (!is.numeric(value) || length(value) != 1) {
    stop("value must be a single integer")
  }

  value <- as.integer(value)
  matches <- which(enum_type@values == value)

  if (length(matches) == 0) {
    NA_character_
  } else {
    names(enum_type@values)[matches[1]]
  }
}


#' Validate FFI call prerequisites
#'
#' Performs comprehensive validation of FFI call inputs before making the call.
#' This helps diagnose issues that would otherwise cause crashes.
#'
#' @param cif CIF object defining the call interface
#' @param symbol NativeSymbol object for the function
#' @param args List of arguments to pass
#' @param verbose Logical; if TRUE, print diagnostic information
#'
#' @return A list with validation results:
#' \describe{
#'   \item{valid}{Logical; TRUE if all checks pass}
#'   \item{errors}{Character vector of error messages (empty if valid)}
#'   \item{warnings}{Character vector of warning messages}
#' }
#'
#' @details
#' This function checks:
#' \itemize{
#'   \item CIF and symbol pointers are not NULL
#'   \item Argument count matches CIF specification
#'   \item No NA values in arguments (unless explicitly allowed)
#'   \item Pointer arguments are not NULL (when applicable)
#' }
#'
#' Note that even with all checks passing, crashes can still occur if:
#' \itemize{
#'   \item The C function signature doesn't match the CIF
#'   \item Pointer arguments point to invalid memory
#'   \item Buffer sizes are incorrect
#'   \item The C function itself has bugs
#' }
#'
#' @export
#' @examples
#' \dontrun{
#' cif <- ffi_cif(ffi_int(), ffi_int(), ffi_int())
#' sym <- ffi_symbol("add_ints")
#' result <- ffi_validate_call(cif, sym, list(1L, 2L))
#' if (result$valid) {
#'   ffi_call(cif, sym, 1L, 2L)
#' }
#' }
ffi_validate_call <- function(cif, symbol, args = list(), verbose = FALSE) {
  errors <- character()
  warnings <- character()

  # Check CIF
  if (!S7::S7_inherits(cif, CIF)) {
    errors <- c(errors, "cif must be a CIF object")
  } else {
    if (ffi_is_null(cif@ref)) {
      errors <- c(errors, "CIF internal pointer is NULL (object may be corrupted)")
    }
  }

  # Check symbol
  if (!S7::S7_inherits(symbol, NativeSymbol)) {
    errors <- c(errors, "symbol must be a NativeSymbol object")
  } else {
    if (ffi_is_null(symbol@address)) {
      errors <- c(errors, sprintf(
        "Symbol '%s' address is NULL (function may not exist or library unloaded)",
        symbol@name
      ))
    }
  }

  # Check argument count
  if (length(errors) == 0) {
    expected_args <- length(cif@arg_types)
    actual_args <- length(args)
    if (actual_args != expected_args) {
      errors <- c(errors, sprintf(
        "Argument count mismatch: CIF expects %d, got %d",
        expected_args, actual_args
      ))
    }
  }

  # Check for NA values
  if (length(errors) == 0 && length(args) > 0) {
    for (i in seq_along(args)) {
      arg <- args[[i]]
      if (is.atomic(arg) && any(is.na(arg))) {
        warnings <- c(warnings, sprintf(
          "Argument %d contains NA values",
          i
        ))
      }
    }
  }

  # Check pointer arguments for NULL
  if (length(errors) == 0 && length(args) > 0) {
    for (i in seq_along(args)) {
      arg <- args[[i]]
      arg_type <- cif@arg_types[[i]]

      # Check if argument is expected to be a pointer
      if (S7::S7_inherits(arg_type, FFIType) &&
        arg_type@name == "pointer") {
        if (inherits(arg, "externalptr") && ffi_is_null(arg)) {
          warnings <- c(warnings, sprintf(
            "Argument %d is a NULL pointer",
            i
          ))
        }
      }
    }
  }

  result <- list(
    valid = length(errors) == 0,
    errors = errors,
    warnings = warnings
  )

  if (verbose) {
    if (result$valid) {
      message("FFI call validation: PASSED")
      if (length(result$warnings) > 0) {
        message("Warnings:")
        for (w in result$warnings) {
          message("  - ", w)
        }
      }
    } else {
      message("FFI call validation: FAILED")
      message("Errors:")
      for (e in result$errors) {
        message("  - ", e)
      }
    }
  }

  result
}
