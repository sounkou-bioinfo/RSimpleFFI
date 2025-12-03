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
#' @param pack Integer packing alignment (NULL for default/natural alignment)
#' @export
StructType <- S7::new_class(
  "StructType",
  package = "RSimpleFFI",
  parent = FFIType,
  properties = list(
    fields = S7::class_character,
    field_types = S7::class_list,
    pack = S7::class_any # NULL or integer
  ),
  validator = function(self) {
    if (length(self@fields) != length(self@field_types)) {
      "Number of field names must match number of field types"
    } else if (
      !all(sapply(self@field_types, function(x) S7::S7_inherits(x, FFIType)))
    ) {
      "All field types must be FFIType objects"
    } else if (!is.null(self@pack) && (!is.numeric(self@pack) || self@pack < 1 || self@pack > 16)) {
      "pack must be NULL or an integer between 1 and 16"
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
# FFIType Union Subclass
#
######################################

# Union type extends FFIType
#' FFI Union Type
#' @name UnionType
#' @param name Character name of the type
#' @param size Integer size in bytes
#' @param ref External pointer to ffi_type
#' @param fields Character vector of field names
#' @param field_types List of FFIType objects for each field
#' @param pack Integer packing alignment (NULL for default/natural alignment)
#' @export
UnionType <- S7::new_class(
  "UnionType",
  package = "RSimpleFFI",
  parent = FFIType,
  properties = list(
    fields = S7::class_character,
    field_types = S7::class_list,
    pack = S7::class_any # NULL or integer
  ),
  validator = function(self) {
    if (length(self@fields) != length(self@field_types)) {
      "Number of field names must match number of field types"
    } else if (
      !all(sapply(self@field_types, function(x) S7::S7_inherits(x, FFIType)))
    ) {
      "All field types must be FFIType objects"
    } else if (!is.null(self@pack) && (!is.numeric(self@pack) || self@pack < 1 || self@pack > 16)) {
      "pack must be NULL or an integer between 1 and 16"
    }
  }
)

#####################################
#
# FFIType Enum Subclass
#
######################################

# Enum type extends FFIType
#' FFI Enumeration Type
#' @name EnumType
#' @param name Character name of the type
#' @param size Integer size in bytes
#' @param ref External pointer to ffi_type
#' @param values Named integer vector of enum values
#' @param underlying_type FFIType for the underlying integer type
#' @export
EnumType <- S7::new_class(
  "EnumType",
  package = "RSimpleFFI",
  parent = FFIType,
  properties = list(
    values = S7::class_integer,
    underlying_type = FFIType
  ),
  validator = function(self) {
    if (is.null(names(self@values)) || any(names(self@values) == "")) {
      "All enum values must be named"
    } else if (!S7::S7_inherits(self@underlying_type, FFIType)) {
      "underlying_type must be an FFIType object"
    }
  }
)


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
# FFI Closure class
#
# Wraps an R function as a C callback
######################################

#' FFI Closure - R function as C callback
#'
#' A closure wraps an R function so it can be used as a callback
#' from C code. The closure has an associated CIF that describes
#' the function signature.
#'
#' @param r_function The R function to wrap
#' @param cif CIF object describing the callback signature
#' @param ref External pointer to the closure
#' @param func_ptr External pointer to the executable function
#' @return An FFIClosure object
#' @keywords Types
#' @export
FFIClosure <- S7::new_class(
  "FFIClosure",
  package = "RSimpleFFI",
  properties = list(
    r_function = S7::class_function,
    cif = CIF,
    ref = S7::class_any,
    func_ptr = S7::class_any
  ),
  validator = function(self) {
    if (!inherits(self@ref, "externalptr")) {
      "@ref must be an external pointer"
    } else if (!inherits(self@func_ptr, "externalptr")) {
      "@func_ptr must be an external pointer"
    }
  }
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

#' void FFI type
#' @return FFIType object for void
#' @keywords Types
#' @export
ffi_void <- function() create_builtin_type("void")

#' int FFI type
#' @return FFIType object for int
#' @keywords Types
#' @export
ffi_int <- function() create_builtin_type("int")

#' double FFI type
#' @return FFIType object for double
#' @keywords Types
#' @export
ffi_double <- function() create_builtin_type("double")

#' float  FFI type
#' @return FFIType object for float
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
#' uint FFI type
#' @return FFIType object for uint
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
#'
#' Creates an FFI structure type from named field types. By default, libffi
#' uses natural alignment (each field aligned to its size). Use the `pack`
#' parameter to specify tighter packing similar to `#pragma pack(n)` in C.
#'
#' @param ... Named FFIType objects representing struct fields
#' @param pack Integer specifying packing alignment (1, 2, 4, 8, or 16), or NULL
#'   for default/natural alignment. When pack=1, fields are byte-aligned (no padding).
#'
#' @section Packing and libffi:
#' The `pack` parameter affects how `ffi_offsetof()`, `ffi_sizeof()`,
#' `ffi_get_field()`, and `ffi_set_field()` calculate field offsets and struct size.
#' This is useful when working with memory buffers that use packed C structs.
#'
#' **Important**: libffi internally always uses natural alignment when passing

#' structs by value in function calls. Packed structs work correctly when:
#' - Reading/writing to memory buffers (ffi_alloc, ffi_get_field, ffi_set_field)
#' - Passing struct pointers to C functions (the C code handles the packed layout)
#' - Computing offsets for manual memory manipulation
#'
#' However, passing a packed struct **by value** to ffi_call may not work correctly
#' because libffi will use natural alignment for the call.
#'
#' @return StructType object
#' @keywords Types
#' @examples
#' # Natural alignment (default)
#' Point <- ffi_struct(x = ffi_int(), y = ffi_int())
#'
#' # Packed struct (1-byte alignment)
#' PackedData <- ffi_struct(
#'   flag = ffi_uint8(),
#'   value = ffi_int32(),
#'   pack = 1
#' )
#'
#' # Check sizes
#' ffi_sizeof(Point) # Natural size
#' ffi_sizeof(PackedData) # Packed size (smaller)
#' @export
ffi_struct <- function(..., pack = NULL) {
  fields <- list(...)

  if (length(fields) == 0) {
    stop("Struct must have at least one field")
  }

  if (is.null(names(fields)) || any(names(fields) == "")) {
    stop("All struct fields must be named")
  }

  # Validate pack parameter
  if (!is.null(pack)) {
    if (!is.numeric(pack) || length(pack) != 1 || pack < 1 || pack > 16) {
      stop("pack must be NULL or an integer between 1 and 16")
    }
    if (!(pack %in% c(1, 2, 4, 8, 16))) {
      stop("pack must be a power of 2: 1, 2, 4, 8, or 16")
    }
    pack <- as.integer(pack)
  }

  # Validate all fields are FFIType objects
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
    field_types = unname(fields),
    pack = pack
  )
}


#' Create FFI union type
#'
#' Creates a union type where all fields share the same memory location.
#' The union's size is the size of its largest member.
#'
#' @param ... Named FFIType objects representing union fields
#' @param pack Integer packing alignment (1, 2, 4, 8, or 16). When specified,
#'   the union's alignment is reduced to min(natural_alignment, pack).
#'   This affects placement when the union is used as a struct member.
#'   Default NULL uses natural alignment.
#' @return UnionType object
#' @keywords Types
#' @examples
#' # Normal union
#' U <- ffi_union(c = ffi_char(), i = ffi_int())
#'
#' # Packed union (alignment = 1)
#' PackedU <- ffi_union(c = ffi_char(), i = ffi_int(), pack = 1)
#'
#' # Packed union in a struct - offset of next field is affected
#' S <- ffi_struct(u = PackedU, after = ffi_char())
#' @export
ffi_union <- function(..., pack = NULL) {
  fields <- list(...)

  if (length(fields) == 0) {
    stop("Union must have at least one field")
  }

  if (is.null(names(fields)) || any(names(fields) == "")) {
    stop("All union fields must be named")
  }

  # Validate pack parameter
  if (!is.null(pack)) {
    if (!is.numeric(pack) || length(pack) != 1 || pack < 1 || pack > 16) {
      stop("pack must be NULL or an integer between 1 and 16")
    }
    if (!(pack %in% c(1, 2, 4, 8, 16))) {
      stop("pack must be a power of 2: 1, 2, 4, 8, or 16")
    }
    pack <- as.integer(pack)
  }

  # Validate all fields are FFIType objects
  if (!all(sapply(fields, function(f) S7::S7_inherits(f, FFIType)))) {
    stop("All union fields must be FFIType objects")
  }

  field_names <- names(fields)
  field_refs <- lapply(fields, function(f) f@ref)

  union_ref <- .Call("R_create_union_ffi_type", field_refs, pack)
  union_size <- .Call("R_get_ffi_type_size", union_ref)

  UnionType(
    name = "union",
    size = union_size,
    ref = union_ref,
    fields = field_names,
    field_types = unname(fields),
    pack = pack
  )
}


#' Create FFI enumeration type
#' @param ... Named integer values representing enum constants
#' @param underlying_type FFIType for underlying integer type (default: ffi_int())
#' @return EnumType object
#' @keywords Types
#' @export
ffi_enum <- function(..., underlying_type = ffi_int()) {
  values <- list(...)

  if (length(values) == 0) {
    stop("Enum must have at least one value")
  }

  if (is.null(names(values)) || any(names(values) == "")) {
    stop("All enum values must be named")
  }

  # Convert list to named integer vector
  values <- setNames(as.integer(unlist(values)), names(values))

  if (!S7::S7_inherits(underlying_type, FFIType)) {
    stop("underlying_type must be an FFIType object")
  }

  EnumType(
    name = "enum",
    size = underlying_type@size,
    ref = underlying_type@ref,
    values = values,
    underlying_type = underlying_type
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

  # For packed structs, use offset-based access
  if (!is.null(struct_type@pack)) {
    offset <- ffi_packed_offset(struct_type, field_index)
    field_type <- struct_type@field_types[[field_index]]
    return(.Call("R_get_struct_field_at_offset", ptr, as.integer(offset), field_type@ref))
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

  # For packed structs, use offset-based access
  if (!is.null(struct_type@pack)) {
    offset <- ffi_packed_offset(struct_type, field)
    field_type <- struct_type@field_types[[field]]
    return(.Call("R_get_struct_field_at_offset", ptr, as.integer(offset), field_type@ref))
  }

  .Call("R_get_struct_field", ptr, as.integer(field - 1), struct_type@ref)
}

#' @export
S7::method(
  ffi_get_field,
  list(S7::class_any, S7::class_character, UnionType)
) <- function(ptr, field, struct_type) {
  field_index <- match(field, struct_type@fields)
  if (is.na(field_index)) {
    stop(
      "No such field '",
      field,
      "' in union. Available fields: ",
      paste(struct_type@fields, collapse = ", ")
    )
  }

  # For unions, all fields share memory at offset 0
  # Use union-specific C function with field type
  field_type <- struct_type@field_types[[field_index]]
  .Call("R_get_union_field", ptr, field_type@ref)
}

#' @export
S7::method(
  ffi_get_field,
  list(S7::class_any, S7::class_integer, UnionType)
) <- function(ptr, field, struct_type) {
  if (field < 1 || field > length(struct_type@fields)) {
    stop(
      "Field index out of range: ",
      field,
      ". Union has ",
      length(struct_type@fields),
      " fields."
    )
  }

  # For unions, all fields share memory at offset 0
  # Use union-specific C function with field type
  field_type <- struct_type@field_types[[field]]
  .Call("R_get_union_field", ptr, field_type@ref)
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

  # For packed structs, use offset-based access
  if (!is.null(struct_type@pack)) {
    offset <- ffi_packed_offset(struct_type, field_index)
    field_type <- struct_type@field_types[[field_index]]
    .Call("R_set_struct_field_at_offset", ptr, as.integer(offset), value, field_type@ref)
    return(invisible(ptr))
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

  # For packed structs, use offset-based access
  if (!is.null(struct_type@pack)) {
    offset <- ffi_packed_offset(struct_type, field)
    field_type <- struct_type@field_types[[field]]
    .Call("R_set_struct_field_at_offset", ptr, as.integer(offset), value, field_type@ref)
    return(invisible(ptr))
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

#' @export
S7::method(
  ffi_set_field,
  list(S7::class_any, S7::class_character, S7::class_any, UnionType)
) <- function(ptr, field, value, struct_type) {
  field_index <- match(field, struct_type@fields)
  if (is.na(field_index)) {
    stop(
      "No such field '",
      field,
      "' in union. Available fields: ",
      paste(struct_type@fields, collapse = ", ")
    )
  }

  # For unions, all fields share memory at offset 0
  # Use union-specific C function with field type
  field_type <- struct_type@field_types[[field_index]]
  .Call(
    "R_set_union_field",
    ptr,
    value,
    field_type@ref
  )
  invisible(ptr)
}

#' @export
S7::method(
  ffi_set_field,
  list(S7::class_any, S7::class_integer, S7::class_any, UnionType)
) <- function(ptr, field, value, struct_type) {
  if (field < 1 || field > length(struct_type@fields)) {
    stop(
      "Field index out of range: ",
      field,
      ". Union has ",
      length(struct_type@fields),
      " fields."
    )
  }

  # For unions, all fields share memory at offset 0
  # Use union-specific C function with field type
  field_type <- struct_type@field_types[[field]]
  .Call(
    "R_set_union_field",
    ptr,
    value,
    field_type@ref
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

#' @export
S7::method(ffi_sizeof, StructType) <- function(type) {
  # For packed structs, compute packed size
  if (!is.null(type@pack)) {
    return(ffi_packed_size(type))
  }
  type@size
}

#' Compute packed size for a struct
#'
#' Computes the total size of a struct using packed alignment rules.
#' The size includes any trailing padding needed for array alignment.
#' Uses C implementation for accurate compiler-matching behavior.
#'
#' @param struct_type StructType object with pack property set
#' @return Integer size in bytes
#' @keywords internal
ffi_packed_size <- function(struct_type) {
  pack <- struct_type@pack
  if (is.null(pack)) {
    return(struct_type@size)
  }

  # Use C function for packed size calculation
  .Call("R_get_packed_struct_size", struct_type@ref, as.integer(pack))
}


#####################################
#
# Field Info and Struct Introspection
#
######################################

#' Field Information Class
#'
#' Represents metadata about a single field in a structure.
#' Field Information Class
#'
#' Contains metadata about a struct field including its name, type,
#' byte offset within the structure, size, and index.
#'
#' @name FieldInfo
#' @param name Character name of the field
#' @param type FFIType of the field
#' @param offset Integer byte offset within structure
#' @param size Integer size of field in bytes
#' @param index Integer 1-based field index
#' @return A FieldInfo object
#' @export
FieldInfo <- S7::new_class(
  "FieldInfo",
  package = "RSimpleFFI",
  properties = list(
    name = S7::class_character,
    type = FFIType,
    offset = S7::class_integer,
    size = S7::class_integer,
    index = S7::class_integer
  )
)

#' @export
S7::method(format, FieldInfo) <- function(x, ...) {
  sprintf(
    "FieldInfo('%s' type=%s, offset=%d, size=%d)",
    x@name, x@type@name, x@offset, x@size
  )
}

#' @export
S7::method(print, FieldInfo) <- function(x, ...) {
  message(format(x), "\n", sep = "")
  invisible(x)
}

#' Get field information from a struct type
#'
#' Returns metadata about a specific field, including its byte offset,
#' size, and type information.
#'
#' @param struct_type StructType object
#' @param field Character field name or integer field index (1-based)
#' @return FieldInfo object with offset, size, alignment info
#'
#' @examples
#' \dontrun{
#' Point <- ffi_struct(x = ffi_int(), y = ffi_double())
#' ffi_field_info(Point, "x")
#' # <FieldInfo 'x' type=int, offset=0, size=4>
#' ffi_field_info(Point, "y")
#' # <FieldInfo 'y' type=double, offset=8, size=8>  (offset 8 due to alignment)
#' }
#'
#' @export
ffi_field_info <- function(struct_type, field) {
  if (!S7::S7_inherits(struct_type, StructType)) {
    stop("struct_type must be a StructType object")
  }

  # Resolve field name to index
  if (is.character(field)) {
    field_index <- match(field, struct_type@fields)
    if (is.na(field_index)) {
      stop(
        "No such field '", field, "' in struct. Available fields: ",
        paste(struct_type@fields, collapse = ", ")
      )
    }
  } else {
    field_index <- as.integer(field)
    if (field_index < 1 || field_index > length(struct_type@fields)) {
      stop("Field index out of range: ", field_index)
    }
  }

  # Get info from C (0-based index)
  info <- .Call("R_get_field_info", struct_type@ref, as.integer(field_index - 1L))

  FieldInfo(
    name = struct_type@fields[field_index],
    type = struct_type@field_types[[field_index]],
    offset = as.integer(info$offset),
    size = as.integer(info$size),
    index = field_index
  )
}

#' Get byte offset of a field in a structure
#'
#' Returns the byte offset of a field within a structure, accounting for
#' alignment requirements. Similar to C's offsetof() macro.
#'
#' For packed structures (created with `pack` parameter), this function computes
#' the offset using the specified packing alignment rather than natural alignment.
#'
#' @param struct_type StructType object
#' @param field Character field name or integer field index (1-based)
#' @param use_pack Logical, whether to use the struct's pack setting. Default TRUE.
#'   Set to FALSE to get libffi's natural alignment offset even for packed structs.
#' @return Integer byte offset
#'
#' @examples
#' \dontrun{
#' Point <- ffi_struct(x = ffi_int(), y = ffi_double())
#' ffi_offsetof(Point, "x") # 0
#' ffi_offsetof(Point, "y") # 8 (aligned to 8-byte boundary)
#'
#' # Packed struct example
#' Packed <- ffi_struct(a = ffi_uint8(), b = ffi_int32(), pack = 1)
#' ffi_offsetof(Packed, "b") # 1 (no padding with pack=1)
#' }
#'
#' @export
ffi_offsetof <- function(struct_type, field, use_pack = TRUE) {
  if (!S7::S7_inherits(struct_type, StructType)) {
    stop("struct_type must be a StructType object")
  }

  # Get field index
  if (is.character(field)) {
    field_index <- match(field, struct_type@fields)
    if (is.na(field_index)) {
      stop("Field '", field, "' not found in struct")
    }
  } else {
    field_index <- as.integer(field)
  }

  # If struct is packed and use_pack is TRUE, compute packed offset
  if (use_pack && !is.null(struct_type@pack)) {
    return(ffi_packed_offset(struct_type, field_index))
  }

  # Otherwise use libffi's natural alignment
  info <- ffi_field_info(struct_type, field)
  info@offset
}

#' Compute packed offset for a field
#'
#' Computes the byte offset of a field using packed alignment rules.
#' This is used internally when a struct has a pack parameter set.
#' Uses C implementation for accurate compiler-matching behavior.
#'
#' @param struct_type StructType object with pack property set
#' @param field_index Integer field index (1-based)
#' @return Integer byte offset
#' @keywords internal
ffi_packed_offset <- function(struct_type, field_index) {
  pack <- struct_type@pack
  if (is.null(pack)) {
    # No packing - use natural alignment via C
    return(.Call(
      "R_get_packed_field_offset", struct_type@ref,
      as.integer(field_index - 1), as.integer(0)
    ))
  }

  # Use C function for packed offset calculation
  .Call(
    "R_get_packed_field_offset", struct_type@ref,
    as.integer(field_index - 1), as.integer(pack)
  )
}

#' Get all field offsets for a struct
#'
#' Returns a named integer vector with byte offsets for all fields.
#' For packed structs, uses the pack alignment setting.
#'
#' @param struct_type StructType object
#' @param use_pack Logical, whether to use the struct's pack setting. Default TRUE.
#' @return Named integer vector of offsets
#'
#' @examples
#' \dontrun{
#' Point <- ffi_struct(x = ffi_int(), y = ffi_double())
#' ffi_all_offsets(Point)
#' # x y
#' # 0 8
#'
#' # Packed struct
#' Packed <- ffi_struct(a = ffi_uint8(), b = ffi_int32(), pack = 1)
#' ffi_all_offsets(Packed)
#' # a b
#' # 0 1
#' }
#'
#' @export
ffi_all_offsets <- function(struct_type, use_pack = TRUE) {
  if (!S7::S7_inherits(struct_type, StructType)) {
    stop("struct_type must be a StructType object")
  }

  # If packed and use_pack, compute packed offsets via C
  if (use_pack && !is.null(struct_type@pack)) {
    offsets <- .Call(
      "R_get_all_packed_field_offsets", struct_type@ref,
      as.integer(struct_type@pack)
    )
    names(offsets) <- struct_type@fields
    return(offsets)
  }

  # Otherwise use libffi's natural alignment
  offsets <- .Call("R_get_all_field_offsets", struct_type@ref)
  names(offsets) <- struct_type@fields
  offsets
}
