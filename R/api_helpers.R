#' Create High-Level API Helpers for Struct
#'
#' This is the main user-facing function for API mode. It generates C code to compute
#' field offsets using the compiler, compiles it, and returns a helper object with
#' constructor and field metadata.
#'
#' @param struct_name Character string naming the struct (e.g., "Point2D")
#' @param field_types Named list of FFIType objects for each field (e.g., list(x = ffi_int(), y = ffi_int()))
#' @param include_dirs Optional character vector of include directories for compilation
#'
#' @return An rffi_struct_helpers S3 object with:
#'   \describe{
#'     \item{$new()}{Constructor function that allocates a new struct}
#'     \item{$fields}{Named list of field metadata (offset + FFIType)}
#'     \item{$get(ptr, field)}{Get field value from struct pointer}
#'     \item{$set(ptr, field, value)}{Set field value in struct pointer}
#'     \item{$lib}{The compiled library object (for cleanup)}
#'   }
#'
#' @examples
#' \dontrun{
#' # Define struct with field types
#' helpers <- ffi_create_helpers(
#'   "Point2D",
#'   list(x = ffi_int(), y = ffi_int())
#' )
#'
#' # Create instance
#' pt <- helpers$new()
#'
#' # Set/get fields
#' helpers$set(pt, "x", 42L)
#' helpers$set(pt, "y", 100L)
#' helpers$get(pt, "x")  # 42L
#' helpers$get(pt, "y")  # 100L
#'
#' # Access field metadata
#' helpers$fields$x$offset  # 0
#' helpers$fields$y$offset  # 4
#' }
#'
#' @export
ffi_create_helpers <- function(struct_name, field_types, include_dirs = NULL) {
  # Validate inputs
  if (!is.character(struct_name) || length(struct_name) != 1) {
    stop("struct_name must be a single character string")
  }
  if (!is.list(field_types) || is.null(names(field_types))) {
    stop("field_types must be a named list")
  }
  if (length(field_types) == 0) {
    stop("field_types must contain at least one field")
  }
  
  # Validate all elements are FFIType objects
  for (i in seq_along(field_types)) {
    if (!S7::S7_inherits(field_types[[i]], FFIType)) {
      stop(sprintf("field_types[[%d]] ('%s') is not an FFIType object",
                   i, names(field_types)[i]))
    }
  }
  
  field_names <- names(field_types)
  
  # Map FFI types to C type names
  c_type_names <- sapply(field_types, function(ft) {
    type_name <- ft@name
    # Map FFI type names to C type names
    c_types <- c(
      "void" = "void",
      "uint8" = "uint8_t",
      "sint8" = "int8_t",
      "uint16" = "uint16_t",
      "sint16" = "int16_t",
      "uint32" = "uint32_t",
      "sint32" = "int32_t",
      "uint64" = "uint64_t",
      "sint64" = "int64_t",
      "float" = "float",
      "double" = "double",
      "pointer" = "void*",
      "longdouble" = "long double",
      "uchar" = "unsigned char",
      "schar" = "signed char",
      "char" = "char",
      "ushort" = "unsigned short",
      "sshort" = "short",
      "short" = "short",
      "uint" = "unsigned int",
      "sint" = "int",
      "int" = "int",
      "ulong" = "unsigned long",
      "slong" = "long",
      "long" = "long"
    )
    
    if (type_name %in% names(c_types)) {
      return(c_types[[type_name]])
    } else {
      # Default fallback
      return("int")
    }
  })
  
  # Generate C code with struct typedef
  c_code <- generate_api_struct_helpers(struct_name, field_names, c_type_names)
  
  # Compile
  lib <- ffi_compile_shlib(c_code, include_dirs)
  
  # Extract functions
  offset_fn_name <- paste0("rffi_", struct_name, "_offsets")
  constructor_fn_name <- paste0("rffi_", struct_name, "_new")
  
  offset_fn <- ffi_get_symbol(lib, offset_fn_name)
  constructor_fn <- ffi_get_symbol(lib, constructor_fn_name)
  
  # Call offset function to get offsets
  offsets <- offset_fn()
  
  # Build field metadata: combine offsets with FFI types
  fields <- list()
  for (field_name in field_names) {
    if (!field_name %in% names(offsets)) {
      stop(sprintf("Field '%s' not found in offset extractor output", field_name))
    }
    fields[[field_name]] <- list(
      offset = offsets[[field_name]],
      type = field_types[[field_name]]
    )
  }
  
  # Create helper object
  structure(
    list(
      struct_name = struct_name,
      new = constructor_fn,
      fields = fields,
      get = function(ptr, field) {
        if (!field %in% names(fields)) {
          stop(sprintf("Unknown field: %s", field))
        }
        field_meta <- fields[[field]]
        # Use direct C call - extract FFI type ref
        field_type_ptr <- if (S7::S7_inherits(field_meta$type, FFIType)) {
          field_meta$type@ref
        } else {
          stop("Invalid field type")
        }
        field_ptr <- .Call("R_struct_get_field_ptr", ptr, field_meta$offset, 
                          field_type_ptr, PACKAGE = "RSimpleFFI")
        .Call("R_field_to_r", field_ptr, PACKAGE = "RSimpleFFI")
      },
      set = function(ptr, field, value) {
        if (!field %in% names(fields)) {
          stop(sprintf("Unknown field: %s", field))
        }
        field_meta <- fields[[field]]
        # Use direct C call - extract FFI type ref
        field_type_ptr <- if (S7::S7_inherits(field_meta$type, FFIType)) {
          field_meta$type@ref
        } else {
          stop("Invalid field type")
        }
        .Call("R_struct_set_field", ptr, field_meta$offset, field_type_ptr, value,
              PACKAGE = "RSimpleFFI")
        invisible(NULL)
      },
      lib = lib
    ),
    class = "rffi_struct_helpers"
  )
}

#' @export
print.rffi_struct_helpers <- function(x, ...) {
  cat(sprintf("<rffi_struct_helpers: %s>\n", x$struct_name))
  cat(sprintf("Fields (%d):\n", length(x$fields)))
  for (field_name in names(x$fields)) {
    field_meta <- x$fields[[field_name]]
    cat(sprintf("  %s: offset=%d, type=%s\n",
                field_name,
                field_meta$offset,
                format(field_meta$type)))
  }
  cat(sprintf("Compiled library: %s\n", x$lib$path))
  invisible(x)
}
