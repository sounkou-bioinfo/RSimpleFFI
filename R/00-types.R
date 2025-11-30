#####################################
#
# FFIType  Basic Types
#
######################################

# Core FFI Type class
#' @name FFIType
#' @title FFI Type representation
#' @param name Character name of the type
#' @param size Integer size in bytes
#' @param ref External pointer to ffi_type
#' @return An FFIType object
#' @keywords Types
#' @export
FFIType <- S7::new_class(
  "FFIType",
  package = "RSimpleFFI",
  properties = list(
    name = S7::class_character,
    size = S7::class_integer,
    ref = S7::class_any
  ),
  validator = function(self) {
    if (length(self@name) != 1) {
      "@name must be length 1"
    } else if (length(self@size) != 1) {
      "@size must be length 1"
    } else if (self@size <= 0) {
      "@size must be positive"
    } else if (!inherits(self@ref, "externalptr")) {
      "@ref must be an external pointer"
    }
  }
)

#####################################
#
# FFIType Struct Subclass
#
######################################

# Structure type extends FFIType
#' FFI Structure Type
#' @name StructType
#' @param name Character name of the type
#' @param size Integer size in bytes
#' @param ref External pointer to ffi_type
#' @param fields Character vector of field names
#' @param field_types List of FFIType objects for each field
#' @export
StructType <- S7::new_class(
  "StructType",
  package = "RSimpleFFI",
  parent = FFIType,
  properties = list(
    fields = S7::class_character,
    field_types = S7::class_list
  ),
  validator = function(self) {
    if (length(self@fields) != length(self@field_types)) {
      "Number of field names must match number of field types"
    } else if (
      !all(sapply(self@field_types, function(x) S7::S7_inherits(x, FFIType)))
    ) {
      "All field types must be FFIType objects"
    }
  }
)

#####################################
#
# FFIType Array Subclass
#
######################################
# Array type extends FFIType
#' FFI Array Type
#' @name ArrayType
#' @param name Character name of the type
#' @param size Integer size in bytes
#' @param ref External pointer to ffi_type
#' @param element_type FFIType of array elements
#' @param length Integer length of array
#' @return An ArrayType object
#' @keywords Types
#' @export
ArrayType <- S7::new_class(
  "ArrayType",
  package = "RSimpleFFI",
  parent = FFIType,
  properties = list(
    element_type = FFIType,
    length = S7::class_integer
  ),
  validator = function(self) {
    if (!S7::S7_inherits(self@element_type, FFIType)) {
      "element_type must be an FFIType object"
    } else if (length(self@length) != 1 || self@length <= 0) {
      "length must be a positive integer"
    }
  }
)

#' Create an FFI array type
#' @param element_type FFIType of array elements
#' @param length Integer length of array
#' @return An ArrayType object
#' @keywords Types
#' @export
ffi_array_type <- function(element_type, length) {
  if (!S7::S7_inherits(element_type, FFIType)) {
    stop("element_type must be an FFIType object")
  }
  if (!is.numeric(length) || length <= 0) {
    stop("length must be a positive integer")
  }
  ref <- .Call("R_create_array_ffi_type", element_type@ref, as.integer(length))
  size <- .Call("R_get_ffi_type_size", ref)

  ArrayType(
    name = paste0("array[", element_type@name, ", ", length, "]"),
    size = size,
    ref = ref,
    element_type = element_type,
    length = as.integer(length)
  )
}


#####################################
#
# CALL Context class
#
######################################

# Call Interface
#' FFI Call Interface (CIF)
#' @param return_type FFIType for return value
#' @param arg_types List of FFIType objects for arguments
#' @param ref External pointer to ffi_cif
#' @return An CIF object
#' @keywords Types
#' @export
CIF <- S7::new_class(
  "CIF",
  package = "RSimpleFFI",
  properties = list(
    return_type = FFIType,
    arg_types = S7::class_list,
    ref = S7::class_any
  ),
  validator = function(self) {
    if (!all(sapply(self@arg_types, function(x) S7::S7_inherits(x, FFIType)))) {
      "All argument types must be FFIType objects"
    }
    if (!S7::S7_inherits(self@return_type, FFIType)) {
      "return_type must be an FFIType object"
    }
  }
)

#####################################
#
# Native Symbol class
#
# TODO : add validation and properties refinement
######################################

#' Native Symbol Reference
#' @param name Character name of the symbol
#' @param address External pointer to the symbol
#' @param library Character name of library (optional)
#' @return A NativeSymbol object
#' @keywords Types
#' @export
NativeSymbol <- S7::new_class(
  "NativeSymbol",
  package = "RSimpleFFI",
  properties = list(
    name = S7::class_character,
    address = S7::class_any,
    library = S7::class_character
  )
)


#####################################
#
# Utility Functions
#
#
######################################

##### Built-in type creation

#' Create built-in FFI type
#'
#' @param name Character name of built-in type
#' @param ... Additional arguments (not used)
#' @return An FFIType object
#' @name create_builtin_type
#' @keywords Types
#' @export
create_builtin_type <- S7::new_generic("create_builtin_type", "name")

#' create_builtin_type
#' @name create_builtin_type
#' @param name Character name of built-in type
#' @param ... Additional arguments (not used)
#' @return FFIType object for bool
#' @keywords Types
#' @export
S7::method(create_builtin_type, S7::class_character) <- function(name) {
  valid_types <- c(
    # Basic types
    "void",
    "int",
    "double",
    "float",
    "pointer",
    "string",
    "raw",
    # Extended integer types
    "int8",
    "int16",
    "int32",
    "int64",
    "uint8",
    "uint16",
    "uint32",
    "uint64",
    # Long types
    "long",
    "ulong",
    "longlong",
    "ulonglong",
    # Short types
    "short",
    "ushort",
    # Char types
    "char",
    "uchar",
    # Unsigned int
    "uint",
    # Floating point variants
    "longdouble",
    # Platform-dependent types
    "size_t",
    "ssize_t",
    "bool",
    "wchar_t"
  )
  if (!name %in% valid_types) {
    stop(
      "Unknown FFI type: ",
      name,
      ". Valid types: ",
      paste(valid_types, collapse = ", ")
    )
  }

  ref <- .Call("R_get_builtin_ffi_type", name)
  if (is.null(ref)) {
    stop("Failed to create FFI type: ", name)
  }

  size <- .Call("R_get_ffi_type_size", ref)
  FFIType(name = name, size = size, ref = ref)
}


#' Char FFI type
#' @return FFIType object for char
#' @keywords Types
#' @export
ffi_raw <- function() create_builtin_type("raw")

#' Double FFI type
#' @return FFIType object for double
#' @keywords Types
#' @export
ffi_void <- function() create_builtin_type("void")

#' Float FFI type
#' @return FFIType object for float
#' @keywords Types
#' @export
ffi_int <- function() create_builtin_type("int")

#' Int FFI type
#' @return FFIType object for int
#' @keywords Types
#' @export
ffi_double <- function() create_builtin_type("double")

#' Int16 FFI type
#' @return FFIType object for int16
#' @keywords Types
#' @export
ffi_float <- function() create_builtin_type("float")

#' pointer FFI type
#' @return FFIType object for pointer
#' @keywords Types
#' @export
ffi_pointer <- function() create_builtin_type("pointer")

#' String FFI type
#' @return FFIType object for string
#' @keywords Types
#' @export
ffi_string <- function() create_builtin_type("string")

# Signed integer types
#' Int8 FFI type
#' @return FFIType object for int8
#' @keywords Types
#' @export
ffi_int8 <- function() create_builtin_type("int8")

#' Int16 FFI type
#' @return FFIType object for int16
#' @keywords Types
#' @export
ffi_int16 <- function() create_builtin_type("int16")

#' Int32 FFI type
#' @return FFIType object for int32
#' @keywords Types
#' @export
ffi_int32 <- function() create_builtin_type("int32")

#' Int64 FFI type
#' @return FFIType object for int64
#' @keywords Types
#' @export
ffi_int64 <- function() create_builtin_type("int64")

#' Char FFI type
#' @return FFIType char type
#' @keywords Types
#' @export
ffi_char <- function() create_builtin_type("char")

#' short FFI type
#' @return FFIType object for short
#' @keywords Types
#' @export
ffi_short <- function() create_builtin_type("short")

#' long FFI type
#' @return FFIType object for long
#' @keywords Types
#' @export
ffi_long <- function() create_builtin_type("long")

#' long long FFI type
#' @return FFIType object for longlong
#' @keywords Types
#' @export
ffi_longlong <- function() create_builtin_type("longlong")

# Unsigned integer types
#' Ssize_t FFI type
#' @return FFIType object for ssize_t
#' @keywords Types
#' @export
ffi_uint <- function() create_builtin_type("uint")

#' Uint8 FFI type
#' @return FFIType object for uint8
#' @keywords Types
#' @export
ffi_uint8 <- function() create_builtin_type("uint8")

#' Uint16 FFI type
#' @return FFIType object for uint16
#' @keywords Types
#' @export
ffi_uint16 <- function() create_builtin_type("uint16")

#' Uint32 FFI type
#' @return FFIType object for uint32
#' @keywords Types
#' @export
ffi_uint32 <- function() create_builtin_type("uint32")

#' Uint64 FFI type
#' @return FFIType object for uint64
#' @keywords Types
#' @export
ffi_uint64 <- function() create_builtin_type("uint64")

#' Uchar FFI type
#' @return FFIType object for uchar
#' @keywords Types
#' @export
ffi_uchar <- function() create_builtin_type("uchar")

#' Ushort FFI type
#' @return FFIType object for ushort
#' @keywords Types
#' @export
ffi_ushort <- function() create_builtin_type("ushort")

#' ulong FFI type
#' @return FFIType object for ulong
#' @keywords Types
#' @export
ffi_ulong <- function() create_builtin_type("ulong")

#' ulonglong FFI type
#' @return FFIType object for ulonglong
#' @keywords Types
#' @export
ffi_ulonglong <- function() create_builtin_type("ulonglong")

# Additional floating point
#' longdouble FFI type
#' @return FFIType object for longdouble
#' @keywords Types
#' @export
ffi_longdouble <- function() create_builtin_type("longdouble")

# Platform-specific size types
#' Size_t FFI type
#' @return FFIType object for size_t
#' @keywords Types
#' @export
ffi_size_t <- function() create_builtin_type("size_t")

# Platform-specific size types
#' ssize_t FFI type
#' @return FFIType object for ssize_t
#' @keywords Types
#' @export
ffi_ssize_t <- function() create_builtin_type("ssize_t")

# Boolean
#' Bool FFI type
#' @return FFIType object for bool
#' @keywords Types
#' @export
ffi_bool <- function() create_builtin_type("bool")

# Wide character
#' Wide char FFI type
#' @name ffi_wchar_t
#' @title Wide char FFI type
#' @return FFIType object for wchar_t
#' @keywords Types
#' @export
ffi_wchar_t <- function() create_builtin_type("wchar_t")


#' Create FFI structure type
#' @param ... Named FFIType objects representing struct fields
#' @return StructType object
#' @keywords Types
#' @export
ffi_struct <- function(...) {
  fields <- list(...)

  if (length(fields) == 0) {
    stop("Struct must have at least one field")
  }

  if (is.null(names(fields)) || any(names(fields) == "")) {
    stop("All struct fields must be named")
  }

  # Validate all fields are FFIType objects
  # TODO : we cannot have nested structs yet really even this check allows it
  if (!all(sapply(fields, function(f) S7::S7_inherits(f, FFIType)))) {
    stop("All struct fields must be FFIType objects")
  }

  field_names <- names(fields)
  field_refs <- lapply(fields, function(f) f@ref)

  struct_ref <- .Call("R_create_struct_ffi_type", field_refs)
  struct_size <- .Call("R_get_ffi_type_size", struct_ref)

  StructType(
    name = "struct",
    size = struct_size,
    ref = struct_ref,
    fields = field_names,
    field_types = unname(fields)
  )
}


#' Get field value from FFI structure
#' @name ffi_get_field
#' @param ptr External pointer to structure
#' @param field Character field name or integer field index
#' @param struct_type StructType object
#' @param ... Additional arguments (not used)
#' @return Field value
#' @keywords Types
#' @export
ffi_get_field <- S7::new_generic(
  "ffi_get_field",
  c("ptr", "field", "struct_type")
)

#' @export
S7::method(
  ffi_get_field,
  list(S7::class_any, S7::class_character, StructType)
) <- function(ptr, field, struct_type) {
  field_index <- match(field, struct_type@fields)
  if (is.na(field_index)) {
    stop(
      "No such field '",
      field,
      "' in struct. Available fields: ",
      paste(struct_type@fields, collapse = ", ")
    )
  }

  .Call("R_get_struct_field", ptr, as.integer(field_index - 1), struct_type@ref)
}

#' @export
S7::method(
  ffi_get_field,
  list(S7::class_any, S7::class_integer, StructType)
) <- function(ptr, field, struct_type) {
  if (field < 1 || field > length(struct_type@fields)) {
    stop(
      "Field index out of range: ",
      field,
      ". Struct has ",
      length(struct_type@fields),
      " fields."
    )
  }

  .Call("R_get_struct_field", ptr, as.integer(field - 1), struct_type@ref)
}

#' Set field value in FFI structure
#' @name ffi_set_field
#' @param ptr External pointer to structure
#' @param field Character field name or integer field index
#' @param value Value to set
#' @param struct_type StructType object
#' @param ... Additional arguments (not used)
#' @return Updated pointer
#' @keywords Types
#' @export
ffi_set_field <- S7::new_generic(
  "ffi_set_field",
  c("ptr", "field", "value", "struct_type")
)

#' @export
S7::method(
  ffi_set_field,
  list(S7::class_any, S7::class_character, S7::class_any, StructType)
) <- function(ptr, field, value, struct_type) {
  field_index <- match(field, struct_type@fields)
  if (is.na(field_index)) {
    stop(
      "No such field '",
      field,
      "' in struct. Available fields: ",
      paste(struct_type@fields, collapse = ", ")
    )
  }

  .Call(
    "R_set_struct_field",
    ptr,
    as.integer(field_index - 1),
    value,
    struct_type@ref
  )
  invisible(ptr)
}

#' @export
S7::method(
  ffi_set_field,
  list(S7::class_any, S7::class_integer, S7::class_any, StructType)
) <- function(ptr, field, value, struct_type) {
  if (field < 1 || field > length(struct_type@fields)) {
    stop(
      "Field index out of range: ",
      field,
      ". Struct has ",
      length(struct_type@fields),
      " fields."
    )
  }

  .Call(
    "R_set_struct_field",
    ptr,
    as.integer(field - 1),
    value,
    struct_type@ref
  )
  invisible(ptr)
}


# Type information
#' Get size of FFI type in bytes
#' @name ffi_sizeof
#' @param type FFIType object
#' @param ... Additional arguments (not used)
#' @return Size in bytes
#' @keywords Types
#' @export
ffi_sizeof <- S7::new_generic("ffi_sizeof", "type")

#' @export
S7::method(ffi_sizeof, FFIType) <- function(type) type@size
