# Bit-field Helper Functions for Manual Bit Manipulation

#' Pack bit-fields into a 64-bit value
#'
#' Packs multiple values into a single 64-bit integer (returned as double)
#' according to specified bit widths. This version uses C code for full
#' 64-bit support.
#'
#' @param values Integer vector of values to pack
#' @param widths Integer vector of bit widths for each value
#' @return Packed value as double (for 64-bit range)
#'
#' @details
#' Values are packed from LSB to MSB (least significant bit to most significant bit).
#' Each value is masked to its specified width and shifted into position.
#' Uses C implementation for full 64-bit support (R integers are only 32-bit).
#'
#' @examples
#' # Pack three bit-fields: enabled (1 bit), mode (3 bits), priority (4 bits)
#' packed <- ffi_pack_bits64(c(1L, 5L, 12L), c(1L, 3L, 4L))
#'
#' # Works with values > 32 bits total
#' large_packed <- ffi_pack_bits64(c(1, 0x7FFFFFFF), c(1L, 31L))
#'
#' @export
ffi_pack_bits64 <- function(values, widths) {
  if (length(values) != length(widths)) {
    stop("Length of values must match length of widths")
  }
  if (any(widths <= 0)) {
    stop("All widths must be positive")
  }
  if (sum(widths) > 64) {
    stop("Total bit width exceeds 64 bits")
  }
  .Call("R_ffi_pack_bits64", as.integer(values), as.integer(widths))
}

#' Unpack bit-fields from a 64-bit value
#'
#' Extracts multiple values from a packed 64-bit value according to specified
#' bit widths.
#'
#' @param packed_value Packed value (as double for 64-bit range)
#' @param widths Integer vector of bit widths for each field
#' @return Integer vector of unpacked values
#'
#' @examples
#' packed <- ffi_pack_bits64(c(1L, 5L, 12L), c(1L, 3L, 4L))
#' ffi_unpack_bits64(packed, c(1L, 3L, 4L)) # c(1, 5, 12)
#'
#' @export
ffi_unpack_bits64 <- function(packed_value, widths) {
  if (any(widths <= 0)) {
    stop("All widths must be positive")
  }
  .Call("R_ffi_unpack_bits64", as.double(packed_value), as.integer(widths))
}

#' Extract a single bit-field from a 64-bit packed value
#'
#' @param packed_value Packed value (as double for 64-bit range)
#' @param bit_offset Bit offset from LSB (0-based)
#' @param bit_width Number of bits in the field
#' @return Extracted value as double (for 64-bit range)
#'
#' @examples
#' packed <- ffi_pack_bits64(c(1L, 5L, 12L), c(1L, 3L, 4L))
#' ffi_extract_bits64(packed, 1L, 3L) # 5 (mode field)
#'
#' @export
ffi_extract_bits64 <- function(packed_value, bit_offset, bit_width) {
  if (bit_width <= 0) stop("bit_width must be positive")
  if (bit_offset < 0) stop("bit_offset must be non-negative")
  .Call(
    "R_ffi_extract_bits64", as.double(packed_value),
    as.integer(bit_offset), as.integer(bit_width)
  )
}

#' Extract a signed bit-field from a 64-bit packed value
#'
#' Extracts a bit-field and sign-extends it based on the high bit.
#' Useful for signed integer fields in C structures.
#'
#' @param packed_value Packed value (as double for 64-bit range)
#' @param bit_offset Bit offset from LSB (0-based)
#' @param bit_width Number of bits in the field
#' @return Extracted signed value as double
#'
#' @examples
#' # Pack a negative value in 4-bit signed field (-3 = 0xD in 4 bits)
#' packed <- ffi_pack_bits64(c(13L), c(4L)) # 0xD = -3 as signed 4-bit
#' ffi_extract_signed_bits64(packed, 0L, 4L) # -3
#'
#' @export
ffi_extract_signed_bits64 <- function(packed_value, bit_offset, bit_width) {
  if (bit_width <= 0) stop("bit_width must be positive")
  if (bit_offset < 0) stop("bit_offset must be non-negative")
  .Call(
    "R_ffi_extract_signed_bits64", as.double(packed_value),
    as.integer(bit_offset), as.integer(bit_width)
  )
}

#' Set a single bit-field in a 64-bit packed value
#'
#' @param packed_value Packed value (as double for 64-bit range)
#' @param new_value New value for the bit-field
#' @param bit_offset Bit offset from LSB (0-based)
#' @param bit_width Number of bits in the field
#' @return Modified packed value as double
#'
#' @examples
#' packed <- ffi_pack_bits64(c(1L, 5L, 12L), c(1L, 3L, 4L))
#' new_packed <- ffi_set_bits64(packed, 7L, 1L, 3L) # Set mode to 7
#' ffi_extract_bits64(new_packed, 1L, 3L) # 7
#'
#' @export
ffi_set_bits64 <- function(packed_value, new_value, bit_offset, bit_width) {
  if (bit_width <= 0) stop("bit_width must be positive")
  if (bit_offset < 0) stop("bit_offset must be non-negative")
  .Call(
    "R_ffi_set_bits64", as.double(packed_value), as.double(new_value),
    as.integer(bit_offset), as.integer(bit_width)
  )
}


#' Pack bit-fields into an integer
#'
#' Packs multiple values into a single integer according to specified bit widths.
#' This is useful when working with C structures that use bit-fields, which are
#' not directly supported by libffi.
#'
#' @param values Integer vector of values to pack
#' @param widths Integer vector of bit widths for each value
#' @param base_type FFI type for the result (default: ffi_uint32())
#' @return Packed integer value
#'
#' @details
#' Values are packed from LSB to MSB (least significant bit to most significant bit).
#' Each value is masked to its specified width and shifted into position.
#'
#' @examples
#' # Pack three bit-fields: enabled (1 bit), mode (3 bits), priority (4 bits)
#' packed <- ffi_pack_bits(c(1L, 5L, 12L), c(1L, 3L, 4L))
#' # Result: 0b1100101 = 0x65 = 101
#'
#' # Verify by unpacking
#' ffi_unpack_bits(packed, c(1L, 3L, 4L))
#' # [1]  1  5 12
#'
#' @export
ffi_pack_bits <- function(values, widths, base_type = ffi_uint32()) {
  if (length(values) != length(widths)) {
    stop("Length of values must match length of widths")
  }

  if (any(widths <= 0)) {
    stop("All widths must be positive")
  }

  # Calculate total bits needed
  total_bits <- sum(widths)
  max_bits <- base_type@size * 8

  if (total_bits > max_bits) {
    stop(sprintf(
      "Total bit width (%d) exceeds base type size (%d bits)",
      total_bits, max_bits
    ))
  }

  result <- 0L
  bit_offset <- 0L

  for (i in seq_along(values)) {
    value <- as.integer(values[i])
    width <- as.integer(widths[i])

    # Create mask for this width
    mask <- bitwShiftL(1L, width) - 1L

    # Mask the value and shift into position
    masked_value <- bitwAnd(value, mask)
    shifted_value <- bitwShiftL(masked_value, bit_offset)

    # Or into result
    result <- bitwOr(result, shifted_value)

    bit_offset <- bit_offset + width
  }

  result
}


#' Unpack bit-fields from an integer
#'
#' Extracts multiple values from a packed integer according to specified bit widths.
#' This is the inverse operation of \code{ffi_pack_bits}.
#'
#' @param packed_value Integer value containing packed bit-fields
#' @param widths Integer vector of bit widths for each field
#' @return Integer vector of unpacked values
#'
#' @details
#' Values are unpacked from LSB to MSB (least significant bit to most significant bit).
#' Each value is extracted by shifting and masking according to its width.
#'
#' @examples
#' # Unpack a value with three bit-fields
#' values <- ffi_unpack_bits(0x65L, c(1L, 3L, 4L))
#' values # [1]  1  5 12
#'
#' # Round-trip test
#' packed <- ffi_pack_bits(c(1L, 5L, 12L), c(1L, 3L, 4L))
#' identical(ffi_unpack_bits(packed, c(1L, 3L, 4L)), c(1L, 5L, 12L))
#'
#' @export
ffi_unpack_bits <- function(packed_value, widths) {
  if (any(widths <= 0)) {
    stop("All widths must be positive")
  }

  packed_value <- as.integer(packed_value)
  result <- integer(length(widths))
  bit_offset <- 0L

  for (i in seq_along(widths)) {
    width <- as.integer(widths[i])

    # Create mask for this width
    mask <- bitwShiftL(1L, width) - 1L

    # Shift value right to position and mask
    shifted_value <- bitwShiftR(packed_value, bit_offset)
    result[i] <- bitwAnd(shifted_value, mask)

    bit_offset <- bit_offset + width
  }

  result
}


#' Extract a single bit-field from a packed value
#'
#' Extracts a single bit-field value from a packed integer at a specified
#' bit offset and width.
#'
#' @param packed_value Integer value containing packed bit-fields
#' @param bit_offset Bit offset from LSB (0-based)
#' @param bit_width Number of bits in the field
#' @return Extracted integer value
#'
#' @examples
#' # Extract 3-bit mode field at bit offset 1 from value 0x65
#' ffi_extract_bit_field(0x65L, 1L, 3L) # 5
#'
#' # Extract 4-bit priority field at bit offset 4
#' ffi_extract_bit_field(0x65L, 4L, 4L) # 6
#'
#' @export
ffi_extract_bit_field <- function(packed_value, bit_offset, bit_width) {
  if (bit_width <= 0) {
    stop("bit_width must be positive")
  }

  if (bit_offset < 0) {
    stop("bit_offset must be non-negative")
  }

  packed_value <- as.integer(packed_value)
  bit_offset <- as.integer(bit_offset)
  bit_width <- as.integer(bit_width)

  # Create mask
  mask <- bitwShiftL(1L, bit_width) - 1L

  # Shift and mask
  shifted <- bitwShiftR(packed_value, bit_offset)
  bitwAnd(shifted, mask)
}


#' Extract a signed bit-field from a packed value
#'
#' Extracts a single bit-field and sign-extends it based on the high bit.
#' This is the 32-bit version using pure R code.
#'
#' @param packed_value Integer value containing packed bit-fields
#' @param bit_offset Bit offset from LSB (0-based)
#' @param bit_width Number of bits in the field
#' @return Extracted signed integer value
#'
#' @examples
#' # A 4-bit value of 13 (0xD) represents -3 in signed 4-bit
#' packed <- ffi_pack_bits(c(13L), c(4L))
#' ffi_extract_signed_bit_field(packed, 0L, 4L) # -3
#'
#' # 3-bit value of 7 represents -1 in signed 3-bit
#' packed2 <- ffi_pack_bits(c(7L), c(3L))
#' ffi_extract_signed_bit_field(packed2, 0L, 3L) # -1
#'
#' @export
ffi_extract_signed_bit_field <- function(packed_value, bit_offset, bit_width) {
  if (bit_width <= 0) {
    stop("bit_width must be positive")
  }
  if (bit_offset < 0) {
    stop("bit_offset must be non-negative")
  }
  if (bit_width > 31) {
    stop("bit_width must be <= 31 for 32-bit signed extraction; use ffi_extract_signed_bits64 for larger")
  }

  # Extract unsigned value first
  unsigned_val <- ffi_extract_bit_field(packed_value, bit_offset, bit_width)


  # Sign extend if the high bit is set
  sign_bit <- bitwShiftL(1L, bit_width - 1L)
  if (bitwAnd(unsigned_val, sign_bit) != 0L) {
    # High bit is set - value is negative
    # Compute: value - 2^width
    unsigned_val <- unsigned_val - bitwShiftL(1L, bit_width)
  }

  unsigned_val
}


#' Set a single bit-field in a packed value
#'
#' Updates a single bit-field in a packed integer at a specified bit offset
#' and width, returning the modified packed value.
#'
#' @param packed_value Integer value containing packed bit-fields
#' @param new_value New value for the bit-field
#' @param bit_offset Bit offset from LSB (0-based)
#' @param bit_width Number of bits in the field
#' @return Modified packed integer value
#'
#' @examples
#' # Set 3-bit mode field at bit offset 1 to value 7
#' ffi_set_bit_field(0x65L, 7L, 1L, 3L) # 0x6F
#'
#' # Set 1-bit enabled field at bit offset 0 to 0
#' ffi_set_bit_field(0x65L, 0L, 0L, 1L) # 0x64
#'
#' @export
ffi_set_bit_field <- function(packed_value, new_value, bit_offset, bit_width) {
  if (bit_width <= 0) {
    stop("bit_width must be positive")
  }

  if (bit_offset < 0) {
    stop("bit_offset must be non-negative")
  }

  packed_value <- as.integer(packed_value)
  new_value <- as.integer(new_value)
  bit_offset <- as.integer(bit_offset)
  bit_width <- as.integer(bit_width)

  # Create mask for the field
  field_mask <- bitwShiftL(1L, bit_width) - 1L

  # Mask the new value
  masked_value <- bitwAnd(new_value, field_mask)

  # Create mask for clearing the field in packed value
  clear_mask <- bitwNot(bitwShiftL(field_mask, bit_offset))

  # Clear the field and set new value
  cleared <- bitwAnd(packed_value, clear_mask)
  result <- bitwOr(cleared, bitwShiftL(masked_value, bit_offset))

  result
}


#' Create accessor functions for a bit-field structure
#'
#' Generates getter and setter functions for a C structure with bit-fields,
#' allowing easy manipulation of packed bit-field values.
#'
#' @param field_specs Named list where names are field names and values are
#'   bit widths (integers)
#' @param base_type FFI type for the packed representation (default: ffi_uint32())
#' @return List with \code{pack}, \code{unpack}, \code{get}, and \code{set} functions
#'
#' @examples
#' # Define a bit-field structure
#' # C equivalent:
#' # struct Flags {
#' #   unsigned int enabled : 1;
#' #   unsigned int mode : 3;
#' #   unsigned int priority : 4;
#' # };
#'
#' flags_accessors <- ffi_create_bitfield_accessors(
#'   list(enabled = 1L, mode = 3L, priority = 4L)
#' )
#'
#' # Pack values
#' packed <- flags_accessors$pack(list(enabled = 1L, mode = 5L, priority = 12L))
#'
#' # Unpack to list
#' flags_accessors$unpack(packed)
#' # $enabled:  1
#' # $mode:     5
#' # $priority: 12
#'
#' # Get a single field
#' flags_accessors$get(packed, "mode") # 5
#'
#' # Set a single field
#' new_packed <- flags_accessors$set(packed, "mode", 7L)
#' flags_accessors$get(new_packed, "mode") # 7
#'
#' @export
ffi_create_bitfield_accessors <- function(field_specs, base_type = ffi_uint32()) {
  # Validate input
  if (!is.list(field_specs) || is.null(names(field_specs))) {
    stop("field_specs must be a named list")
  }

  if (any(names(field_specs) == "")) {
    stop("All fields must be named")
  }

  field_names <- names(field_specs)
  field_widths <- as.integer(unlist(field_specs))

  if (any(field_widths <= 0)) {
    stop("All field widths must be positive")
  }

  # Calculate bit offsets
  field_offsets <- integer(length(field_widths))
  if (length(field_widths) > 0) {
    field_offsets[1] <- 0L
    if (length(field_widths) > 1) {
      for (i in 2:length(field_widths)) {
        field_offsets[i] <- field_offsets[i - 1] + field_widths[i - 1]
      }
    }
  }

  # Create lookup for field index by name
  field_index <- stats::setNames(seq_along(field_names), field_names)

  list(
    # Pack a named list into an integer
    pack = function(values) {
      if (!is.list(values)) {
        stop("values must be a named list")
      }

      int_values <- integer(length(field_names))
      for (i in seq_along(field_names)) {
        fname <- field_names[i]
        if (fname %in% names(values)) {
          int_values[i] <- as.integer(values[[fname]])
        } else {
          int_values[i] <- 0L
        }
      }

      ffi_pack_bits(int_values, field_widths, base_type)
    },

    # Unpack an integer into a named list
    unpack = function(packed_value) {
      values <- ffi_unpack_bits(packed_value, field_widths)
      stats::setNames(as.list(values), field_names)
    },

    # Get a single field by name
    get = function(packed_value, field_name) {
      if (!field_name %in% field_names) {
        stop(sprintf("Unknown field: %s", field_name))
      }
      idx <- field_index[[field_name]]
      ffi_extract_bit_field(packed_value, field_offsets[idx], field_widths[idx])
    },

    # Set a single field by name
    set = function(packed_value, field_name, new_value) {
      if (!field_name %in% field_names) {
        stop(sprintf("Unknown field: %s", field_name))
      }
      idx <- field_index[[field_name]]
      ffi_set_bit_field(packed_value, new_value, field_offsets[idx], field_widths[idx])
    }
  )
}
