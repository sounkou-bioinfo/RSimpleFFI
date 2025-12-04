/*
 * simple_ffi.c
 *
 * Core FFI type and memory management for RSimpleFFI
 *
 * Author: Sounkou Mahamane Toure
 * Licensed under GPL-3
 */

#include <R.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h> 
#include "ffi.h"
#include <string.h>
#include <stdint.h>
#include <wchar.h>
#include <math.h>
#if defined(FFI_VERSION_NUMBER)
#  if FFI_VERSION_NUMBER < 30000
#    error "libffi >= 3.0.0 is required"
#  endif
#endif

// Forward declarations for packed struct support
static size_t calculate_packed_struct_size(ffi_type* struct_type, int pack);



/*
*
*
* Bunch of type definitions and mappings
* 
* The structs are used to configure call context for libffi
*/




typedef struct {
    const char* name;
    ffi_type* type; // pointer to libffi type object
} ffi_type_map_t;



// Global table of built-in FFI types
// Custom FFI type objects for platform-dependent types
// these are static variables that should never be freed

static ffi_type ffi_type_size_t_custom = { 
    sizeof(size_t), 
    sizeof(void*),  // alignment 
    sizeof(size_t) == sizeof(long) ? FFI_TYPE_SINT64 : FFI_TYPE_SINT32, 
    NULL 
};

// Raw type for R raw vectors (byte arrays)
#ifdef FFI_TYPE_UINT8
static ffi_type ffi_type_raw_custom = {
    sizeof(char),
    sizeof(char),
    FFI_TYPE_UINT8,
    NULL
};
#else
static ffi_type ffi_type_raw_custom = {
    sizeof(char),
    sizeof(char),
    FFI_TYPE_SINT8,
    NULL
};
#endif

static ffi_type ffi_type_ssize_t_custom = { 
    sizeof(ssize_t), 
    sizeof(void*),  // alignment 
    sizeof(ssize_t) == sizeof(long) ? FFI_TYPE_SINT64 : FFI_TYPE_SINT32, 
    NULL 
};

// C99 _Bool type - typically 1 byte
// Note: C's stdbool.h bool type maps to _Bool
static ffi_type ffi_type_bool_custom = { 
    sizeof(_Bool), 
    sizeof(_Bool),  // alignment 
    FFI_TYPE_UINT8,  // _Bool is essentially unsigned with value 0 or 1
    NULL 
};

static ffi_type ffi_type_wchar_t_custom = { 
    sizeof(wchar_t), 
    sizeof(wchar_t),  // alignment 
    sizeof(wchar_t) == sizeof(int) ? FFI_TYPE_SINT32 : FFI_TYPE_SINT16, 
    NULL 
};

// Custom string type - same as pointer but semantically different
static ffi_type ffi_type_string_custom = { 
    sizeof(char*), 
    sizeof(void*),  // alignment 
    FFI_TYPE_POINTER, 
    NULL 
};


static ffi_type_map_t builtin_ffi_types[] = {
    {"void", &ffi_type_void},
    {"int", &ffi_type_sint32},
    {"double", &ffi_type_double},
    {"float", &ffi_type_float},
    {"pointer", &ffi_type_pointer},
    {"string", &ffi_type_string_custom},
    {"raw", &ffi_type_raw_custom},
    
    // Extended integer types
    {"int8", &ffi_type_sint8},
    {"int16", &ffi_type_sint16},
    {"int32", &ffi_type_sint32},
    {"int64", &ffi_type_sint64},
    {"uint8", &ffi_type_uint8},
    {"uint16", &ffi_type_uint16},
    {"uint32", &ffi_type_uint32},
    {"uint64", &ffi_type_uint64},
    
    // Long types (map to appropriate sized types)
    {"long", sizeof(long) == 8 ? &ffi_type_sint64 : &ffi_type_sint32},
    {"ulong", sizeof(unsigned long) == 8 ? &ffi_type_uint64 : &ffi_type_uint32},
    
    // Floating point variants
    {"longdouble", &ffi_type_longdouble},
    
    // Platform-dependent types
    {"size_t", &ffi_type_size_t_custom},
    {"ssize_t", &ffi_type_ssize_t_custom}, 
    {"bool", &ffi_type_bool_custom},
    {"wchar_t", &ffi_type_wchar_t_custom},
    
    // Missing ctypes compatibility types
    // some typdefs are required here
    {"char", &ffi_type_sint8},           // c_char
    {"uchar", &ffi_type_uint8},          // c_ubyte  
    {"short", &ffi_type_sint16},         // c_short
    {"ushort", &ffi_type_uint16},        // c_ushort
    {"uint", &ffi_type_uint32},          // c_uint
    {"longlong", sizeof(long long) == 8 ? &ffi_type_sint64 : &ffi_type_sint32},  // c_longlong
    {"ulonglong", sizeof(unsigned long long) == 8 ? &ffi_type_uint64 : &ffi_type_uint32}, // c_ulonglong
    {NULL, NULL}
};


/*
*
* NA value detection
*
* R's NA values are problematic when passed to C:
* - NA_INTEGER is INT_MIN (-2147483648) - a valid C integer
* - NA_LOGICAL is also INT_MIN
* - NA_STRING would pass garbage/NULL pointer
* - NA_REAL is a special NaN that C treats as regular NaN (safe, but may be unintended)
*
* NaN (not NA) has well-defined IEEE 754 semantics and is allowed through.
*/

// Check if a scalar R value contains NA (not NaN)
// Returns 1 if NA detected, 0 otherwise
static int has_na_value(SEXP r_val) {
    if (r_val == R_NilValue) {
        return 0;  // NULL is handled separately, not an NA
    }
    
    // Check type first - external pointers, environments, etc. cannot contain NA
    // and don't support LENGTH()
    int type = TYPEOF(r_val);
    if (type == EXTPTRSXP || type == ENVSXP || type == CLOSXP || 
        type == LISTSXP || type == LANGSXP || type == NILSXP ||
        type == SYMSXP || type == BUILTINSXP || type == SPECIALSXP) {
        return 0;
    }
    
    int n = LENGTH(r_val);
    if (n == 0) {
        return 0;
    }
    
    switch (type) {
        case INTSXP:
            return INTEGER(r_val)[0] == NA_INTEGER;
        case REALSXP:
            // Use ISNA to check only for R's NA, not regular NaN
            // NaN has well-defined C semantics and is allowed
            return ISNA(REAL(r_val)[0]);
        case LGLSXP:
            return LOGICAL(r_val)[0] == NA_LOGICAL;
        case STRSXP:
            return STRING_ELT(r_val, 0) == NA_STRING;
        case RAWSXP:
            return 0;  // Raw vectors cannot contain NA
        default:
            return 0;
    }
}


/*

*
*
* Type creation and management
*
*

*/

// Get built-in FFI type by name
SEXP R_get_builtin_ffi_type(SEXP r_name) {
    const char* name = CHAR(STRING_ELT(r_name, 0));
    
    for (int i = 0; builtin_ffi_types[i].name; i++) {
        if (strcmp(name, builtin_ffi_types[i].name) == 0) {
            // we let these leak
            SEXP extPtr = R_MakeExternalPtr(builtin_ffi_types[i].type, R_NilValue, R_NilValue);
            return extPtr;
        }
    }
    
    return R_NilValue;
}

// Get FFI type size in bytes
SEXP R_get_ffi_type_size(SEXP r_type) {
    ffi_type* type = (ffi_type*)R_ExternalPtrAddr(r_type);
    if (!type) {
        Rf_error("Invalid FFI type pointer");
    }
    return ScalarInteger((int)type->size);
}

// Get FFI type alignment
SEXP R_get_ffi_type_alignment(SEXP r_type) {
    ffi_type* type = (ffi_type*)R_ExternalPtrAddr(r_type);
    if (!type) {
        Rf_error("Invalid FFI type pointer");
    }
    return ScalarInteger((int)type->alignment);
}


static void struct_type_finalizer(SEXP extPtr) {
    ffi_type* struct_type = (ffi_type*)R_ExternalPtrAddr(extPtr);
    if (struct_type) {
        if (struct_type->elements) free(struct_type->elements);
        free(struct_type);
        R_ClearExternalPtr(extPtr);
    }
}

// Create structure FFI type
// Create structure FFI type
// pack: packing alignment (0 or NULL for natural alignment, 1/2/4/8/16 for packed)
SEXP R_create_struct_ffi_type(SEXP r_field_types, SEXP r_pack) {
    int num_fields = LENGTH(r_field_types);
    if (num_fields == 0) {
        Rf_error("Structure must have at least one field");
    }
    
    // Get pack value (0 means natural alignment)
    int pack = 0;
    if (r_pack != R_NilValue && LENGTH(r_pack) > 0) {
        pack = INTEGER(r_pack)[0];
    }
    
    // Allocate array for field types (+ NULL terminator)
    ffi_type** field_types = (ffi_type**)calloc(num_fields + 1, sizeof(ffi_type*));
    if (!field_types) {
        Rf_error("Memory allocation failed");
    }
    
    // Extract field types from R list
    for (int i = 0; i < num_fields; i++) {
        SEXP field = VECTOR_ELT(r_field_types, i);
        field_types[i] = (ffi_type*)R_ExternalPtrAddr(field);
        if (!field_types[i]) {
            free(field_types);
            Rf_error("Invalid field type at index %d", i + 1);
        }
    }
    field_types[num_fields] = NULL;  // NULL terminator
    
    // Create structure type
    ffi_type* struct_type = (ffi_type*)malloc(sizeof(ffi_type));
    if (!struct_type) {
        free(field_types);
        Rf_error("Memory allocation failed");
    }
    
    struct_type->size = 0;  // Will be computed by libffi
    struct_type->alignment = 0;  // Will be computed by libffi  
    struct_type->type = FFI_TYPE_STRUCT;
    struct_type->elements = field_types;
    
    // Create a dummy CIF to force libffi to compute struct size
    ffi_cif dummy_cif;
    ffi_status status = ffi_prep_cif(&dummy_cif, FFI_DEFAULT_ABI, 0, struct_type, NULL);
    if (status != FFI_OK) {
        free(field_types);
        free(struct_type);
        Rf_error("Failed to compute struct layout");
    }
    
    // For packed structs, create a byval type with byte-level elements
    // This ensures libffi marshals the correct bytes when passing by value
    ffi_type* byval_type = NULL;
    
    if (pack > 0) {
        // Calculate packed size
        size_t packed_size = calculate_packed_struct_size(struct_type, pack);
        
        // Create byte-level ffi_type for by-value passing
        // This has N char elements so libffi computes offsets 0,1,2,...,N-1
        ffi_type** byte_elements = (ffi_type**)calloc(packed_size + 1, sizeof(ffi_type*));
        if (!byte_elements) {
            free(field_types);
            free(struct_type);
            Rf_error("Memory allocation failed for byval type");
        }
        
        for (size_t i = 0; i < packed_size; i++) {
            byte_elements[i] = &ffi_type_uint8;
        }
        byte_elements[packed_size] = NULL;
        
        byval_type = (ffi_type*)malloc(sizeof(ffi_type));
        if (!byval_type) {
            free(byte_elements);
            free(field_types);
            free(struct_type);
            Rf_error("Memory allocation failed for byval type");
        }
        
        byval_type->size = 0;
        byval_type->alignment = 0;
        byval_type->type = FFI_TYPE_STRUCT;
        byval_type->elements = byte_elements;
        
        // Prep the byval type
        ffi_cif byval_cif;
        status = ffi_prep_cif(&byval_cif, FFI_DEFAULT_ABI, 0, byval_type, NULL);
        if (status != FFI_OK) {
            free(byte_elements);
            free(byval_type);
            free(field_types);
            free(struct_type);
            Rf_error("Failed to prepare byval type");
        }
        
        // Apply pack to main struct alignment and size
        if (struct_type->alignment > (unsigned short)pack) {
            struct_type->alignment = (unsigned short)pack;
        }
        struct_type->size = packed_size;
    }
    
    // Store byval_type in the external pointer's tag (or NULL if not packed)
    SEXP byval_extPtr = R_NilValue;
    int nprotect = 0;
    
    if (byval_type) {
        byval_extPtr = PROTECT(R_MakeExternalPtr(byval_type, R_NilValue, R_NilValue));
        nprotect++;
        R_RegisterCFinalizerEx(byval_extPtr, struct_type_finalizer, TRUE);
    }
    
    SEXP extPtr = PROTECT(R_MakeExternalPtr(struct_type, byval_extPtr, R_NilValue));
    nprotect++;
    R_RegisterCFinalizerEx(extPtr, struct_type_finalizer, TRUE);
    
    UNPROTECT(nprotect);
    return extPtr;
}

// Get the byval type for a packed struct (for passing by value)
// Returns the byval type external pointer, or R_NilValue if not packed
SEXP R_get_struct_byval_type(SEXP r_struct_type) {
    // The byval type is stored in the tag of the external pointer
    return R_ExternalPtrTag(r_struct_type);
}


// Create union FFI type
// Per libffi manual: emulate union using FFI_TYPE_STRUCT with single element
// that has the size of the largest member and largest alignment
// pack: packing alignment (0 or NULL for natural alignment, 1/2/4/8/16 for packed)
SEXP R_create_union_ffi_type(SEXP r_field_types, SEXP r_pack) {
    int num_fields = LENGTH(r_field_types);
    if (num_fields == 0) {
        Rf_error("Union must have at least one field");
    }
    
    // Get pack value (0 means natural alignment)
    int pack = 0;
    if (r_pack != R_NilValue && LENGTH(r_pack) > 0) {
        pack = INTEGER(r_pack)[0];
    }
    
    // First pass: find largest size and alignment by forcing layout computation
    size_t max_size = 0;
    unsigned short max_alignment = 0;
    ffi_abi desired_abi = FFI_DEFAULT_ABI;
    
    for (int i = 0; i < num_fields; i++) {
        SEXP field = VECTOR_ELT(r_field_types, i);
        ffi_type* field_type = (ffi_type*)R_ExternalPtrAddr(field);
        if (!field_type) {
            Rf_error("Invalid field type at index %d", i + 1);
        }
        
        // Use ffi_prep_cif trick to ensure field type is laid out
        ffi_cif cif;
        if (ffi_prep_cif(&cif, desired_abi, 0, field_type, NULL) == FFI_OK) {
            if (field_type->size > max_size) {
                max_size = field_type->size;
            }
            if (field_type->alignment > max_alignment) {
                max_alignment = field_type->alignment;
            }
        }
    }
    
    // Apply pack to alignment: effective_alignment = min(natural, pack)
    if (pack > 0 && pack < max_alignment) {
        max_alignment = (unsigned short)pack;
    }
    
    // Create a single-element struct with the largest member's properties
    ffi_type** elements = (ffi_type**)calloc(2, sizeof(ffi_type*));
    if (!elements) {
        Rf_error("Memory allocation failed");
    }
    
    // Get the first field type as the representative element
    // (any field could work; we use first for simplicity)
    SEXP first_field = VECTOR_ELT(r_field_types, 0);
    ffi_type* first_type = (ffi_type*)R_ExternalPtrAddr(first_field);
    elements[0] = first_type;
    elements[1] = NULL;
    
    // Create union type as a struct
    ffi_type* union_type = (ffi_type*)malloc(sizeof(ffi_type));
    if (!union_type) {
        free(elements);
        Rf_error("Memory allocation failed");
    }
    
    union_type->size = max_size;
    union_type->alignment = max_alignment;
    union_type->type = FFI_TYPE_STRUCT;
    union_type->elements = elements;
    
    // Force layout computation
    ffi_cif dummy_cif;
    ffi_status status = ffi_prep_cif(&dummy_cif, desired_abi, 0, union_type, NULL);
    if (status != FFI_OK) {
        free(elements);
        free(union_type);
        Rf_error("Failed to compute union layout");
    }
    
    // Verify the union has the expected size and alignment
    // libffi may have overwritten our alignment, so force it back
    if (union_type->size < max_size) {
        union_type->size = max_size;
    }
    if (pack > 0 && union_type->alignment > (unsigned short)pack) {
        union_type->alignment = (unsigned short)pack;
    }
    
    SEXP extPtr = R_MakeExternalPtr(union_type, R_NilValue, R_NilValue);
    return extPtr;
}


/*

*
*
*
* Memory management
*
*/

// Finalizer for freeing malloc'd memory
static void buffer_finalizer(SEXP extPtr) {
    void* ptr = R_ExternalPtrAddr(extPtr);
    if (ptr) free(ptr);
    R_ClearExternalPtr(extPtr);
}

// Explicitly free memory pointed to by an external pointer
// Use this for pointers returned from C that you know should be freed
SEXP R_ffi_free(SEXP r_ptr) {
    if (TYPEOF(r_ptr) != EXTPTRSXP) {
        Rf_error("Expected an external pointer");
    }
    void* ptr = R_ExternalPtrAddr(r_ptr);
    if (ptr) {
        free(ptr);
        R_ClearExternalPtr(r_ptr);
    }
    return R_NilValue;
}

// Allocate a buffer and return an external pointer with finalizer
SEXP R_alloc_buffer(SEXP r_size) {
    R_xlen_t size = asInteger(r_size);
    if (size <= 0) Rf_error("Size must be positive");
    void* buf = malloc(size);
    if (!buf) Rf_error("Memory allocation failed");
    SEXP extPtr = R_MakeExternalPtr(buf, Rf_install("buffer"), R_NilValue);
    R_RegisterCFinalizerEx(extPtr, buffer_finalizer, TRUE);
    return extPtr;
}

// Allocate a buffer for n elements of a given FFI type
SEXP R_alloc_typed_buffer(SEXP r_type, SEXP r_n) {
    ffi_type* type = (ffi_type*)R_ExternalPtrAddr(r_type);
    if (!type) Rf_error("Invalid FFI type pointer");
    int n = asInteger(r_n);
    if (n <= 0) Rf_error("n must be positive");
   // size_t total = (size_t)n * type->size;
    void* buf = calloc(n, type->size);
    if (!buf) Rf_error("Memory allocation failed");
    SEXP extPtr = R_MakeExternalPtr(buf, Rf_install("typed_buffer"), R_NilValue);
    R_RegisterCFinalizerEx(extPtr, buffer_finalizer, TRUE);
    return extPtr;
}


SEXP R_create_array_ffi_type(SEXP r_element_type, SEXP r_length) {
    ffi_type* element_type = (ffi_type*)R_ExternalPtrAddr(r_element_type);
    if (!element_type) Rf_error("Invalid element type pointer");
    int length = asInteger(r_length);
    if (length <= 0) Rf_error("Array length must be positive");
    ffi_type** elements = (ffi_type**)calloc(length + 1, sizeof(ffi_type*));
    if (!elements) Rf_error("Memory allocation failed");
    for (int i = 0; i < length; i++) elements[i] = element_type;
    elements[length] = NULL;
    ffi_type* array_type = (ffi_type*)malloc(sizeof(ffi_type));
    if (!array_type) { free(elements); Rf_error("Memory allocation failed"); }
    array_type->size = 0;
    array_type->alignment = 0;
    array_type->type = FFI_TYPE_STRUCT; // libffi uses STRUCT for arrays
    array_type->elements = elements;
    ffi_cif dummy_cif;
    ffi_status status = ffi_prep_cif(&dummy_cif, FFI_DEFAULT_ABI, 0, array_type, NULL);
    if (status != FFI_OK) { free(elements); free(array_type); Rf_error("Failed to compute array layout"); }
    SEXP extPtr = R_MakeExternalPtr(array_type, R_NilValue, R_NilValue);
    return extPtr;
}


// Fill a typed buffer from an R vector (int or double)
SEXP R_fill_typed_buffer(SEXP r_ptr, SEXP r_vals, SEXP r_type) {
    void* ptr = R_ExternalPtrAddr(r_ptr);
    ffi_type* type = (ffi_type*)R_ExternalPtrAddr(r_type);
    if (!ptr) Rf_error("Invalid pointer");
    if (!type) Rf_error("Invalid FFI type pointer");
    int n = LENGTH(r_vals);
    switch (type->type) {
        case FFI_TYPE_SINT32: {
            int* dest = (int*)ptr;
            for (int i = 0; i < n; i++) dest[i] = INTEGER(r_vals)[i];
            break;
        }
        case FFI_TYPE_DOUBLE: {
            double* dest = (double*)ptr;
            for (int i = 0; i < n; i++) dest[i] = REAL(r_vals)[i];
            break;
        }
        case FFI_TYPE_UINT8:
        // fall through
        case FFI_TYPE_SINT8: {
            if (TYPEOF(r_vals) != RAWSXP)
                Rf_error("For raw/uint8 buffers, supply a raw vector");
            memcpy(ptr, RAW(r_vals), n);
            break;
        }
        default:
            Rf_error("R_fill_typed_buffer only supports int, double, and raw types for now");
    }
    return R_NilValue;
}






/*
*
*
* Conversion between R values and native values
*
* Type conversion follows C99 semantics:
* - Integer promotions: smaller types promote to int
* - Signed/unsigned conversions: modular arithmetic for unsigned types
* - Floating point: standard C conversions with truncation toward zero
*
*/

// Helper: Get integer value from R SEXP (handles INTSXP, REALSXP, LGLSXP)
// Returns 1 on success, 0 on failure. Sets *out_val and *is_exact
static int get_r_integer_value(SEXP r_val, int64_t* out_val, int* is_exact) {
    *is_exact = 1;
    if (TYPEOF(r_val) == INTSXP) {
        *out_val = (int64_t)INTEGER(r_val)[0];
        return 1;
    } else if (TYPEOF(r_val) == REALSXP) {
        double val = REAL(r_val)[0];
        if (!R_FINITE(val)) {
            return 0;  // Inf, -Inf, NaN cannot convert to integer
        }
        // Check if value is exactly representable as integer
        double truncated = trunc(val);
        *is_exact = (val == truncated);
        *out_val = (int64_t)truncated;
        return 1;
    } else if (TYPEOF(r_val) == LGLSXP) {
        *out_val = (int64_t)LOGICAL(r_val)[0];
        return 1;
    }
    return 0;
}

// Helper: Get unsigned integer value from R SEXP
// For uint64 we need special handling since int64_t may not cover full range
static int get_r_unsigned_value(SEXP r_val, uint64_t* out_val, int* is_exact) {
    *is_exact = 1;
    if (TYPEOF(r_val) == INTSXP) {
        int val = INTEGER(r_val)[0];
        // C99: signed to unsigned conversion uses modular arithmetic
        *out_val = (uint64_t)(unsigned int)val;
        return 1;
    } else if (TYPEOF(r_val) == REALSXP) {
        double val = REAL(r_val)[0];
        if (!R_FINITE(val)) {
            return 0;
        }
        double truncated = trunc(val);
        *is_exact = (val == truncated);
        // Handle negative values with modular arithmetic for unsigned conversion
        if (truncated < 0) {
            // C99 6.3.1.3: conversion to unsigned wraps around
            // We need to be careful here for very large negative numbers
            *out_val = (uint64_t)(int64_t)truncated;
        } else {
            *out_val = (uint64_t)truncated;
        }
        return 1;
    } else if (TYPEOF(r_val) == LGLSXP) {
        *out_val = (uint64_t)LOGICAL(r_val)[0];
        return 1;
    }
    return 0;
}

// Helper: Get double value from R SEXP
static int get_r_double_value(SEXP r_val, double* out_val) {
    if (TYPEOF(r_val) == REALSXP) {
        *out_val = REAL(r_val)[0];
        return 1;
    } else if (TYPEOF(r_val) == INTSXP) {
        *out_val = (double)INTEGER(r_val)[0];
        return 1;
    } else if (TYPEOF(r_val) == LGLSXP) {
        *out_val = (double)LOGICAL(r_val)[0];
        return 1;
    }
    return 0;
}

// Convert R value to native value for FFI call
// Follows C99 type conversion semantics
void* convert_r_to_native(SEXP r_val, ffi_type* type) {
    int64_t ival;
    uint64_t uval;
    double dval;
    int is_exact;
    
    // Handle custom types first (before switch on type->type)
    // These custom types may share type codes with standard types
    if (type == &ffi_type_string_custom) {
        if (TYPEOF(r_val) == STRSXP && LENGTH(r_val) > 0) {
            const char* str = CHAR(STRING_ELT(r_val, 0));
            const char** ptr = (const char**)R_alloc(1, sizeof(const char*));
            *ptr = str;
            return ptr;
        } else if (r_val == R_NilValue) {
            static const char* null_str = NULL;
            return (void*)&null_str;
        } else {
            Rf_error("Cannot convert to string (expected character vector or NULL)");
        }
    } else if (type == &ffi_type_size_t_custom) {
        size_t* converted = (size_t*)R_alloc(1, sizeof(size_t));
        if (!get_r_unsigned_value(r_val, &uval, &is_exact)) {
            Rf_error("Cannot convert to size_t: unsupported R type");
        }
        *converted = (size_t)uval;
        return converted;
    } else if (type == &ffi_type_ssize_t_custom) {
        ssize_t* converted = (ssize_t*)R_alloc(1, sizeof(ssize_t));
        if (!get_r_integer_value(r_val, &ival, &is_exact)) {
            Rf_error("Cannot convert to ssize_t: unsupported R type");
        }
        *converted = (ssize_t)ival;
        return converted;
    } else if (type == &ffi_type_bool_custom) {
        // C99 _Bool type
        _Bool* converted = (_Bool*)R_alloc(1, sizeof(_Bool));
        if (TYPEOF(r_val) == LGLSXP) {
            *converted = (_Bool)(LOGICAL(r_val)[0] != 0);
        } else if (!get_r_integer_value(r_val, &ival, &is_exact)) {
            Rf_error("Cannot convert to bool: unsupported R type");
        } else {
            *converted = (_Bool)(ival != 0);
        }
        return converted;
    } else if (type == &ffi_type_wchar_t_custom) {
        wchar_t* converted = (wchar_t*)R_alloc(1, sizeof(wchar_t));
        if (TYPEOF(r_val) == STRSXP && LENGTH(r_val) > 0) {
            const char* str = CHAR(STRING_ELT(r_val, 0));
            if (strlen(str) > 0) {
                *converted = (wchar_t)(unsigned char)str[0];
            } else {
                *converted = 0;
            }
        } else if (get_r_integer_value(r_val, &ival, &is_exact)) {
            *converted = (wchar_t)ival;
        } else {
            Rf_error("Cannot convert to wchar_t: unsupported R type");
        }
        return converted;
    } else if (type == &ffi_type_raw_custom) {
        uint8_t* converted = (uint8_t*)R_alloc(1, sizeof(uint8_t));
        if (TYPEOF(r_val) == RAWSXP && LENGTH(r_val) > 0) {
            *converted = RAW(r_val)[0];
        } else if (!get_r_unsigned_value(r_val, &uval, &is_exact)) {
            Rf_error("Cannot convert to raw: unsupported R type");
        } else {
            *converted = (uint8_t)uval;
        }
        return converted;
    }
    
    // Handle standard libffi types
    switch (type->type) {
        case FFI_TYPE_VOID:
            return NULL;
            
        case FFI_TYPE_SINT8: {
            int8_t* converted = (int8_t*)R_alloc(1, sizeof(int8_t));
            if (!get_r_integer_value(r_val, &ival, &is_exact)) {
                Rf_error("Cannot convert to int8: unsupported R type");
            }
            // C99: conversion to signed type is implementation-defined if out of range
            // We use modular arithmetic (common behavior)
            *converted = (int8_t)ival;
            return converted;
        }
            
        case FFI_TYPE_SINT16: {
            int16_t* converted = (int16_t*)R_alloc(1, sizeof(int16_t));
            if (!get_r_integer_value(r_val, &ival, &is_exact)) {
                Rf_error("Cannot convert to int16: unsupported R type");
            }
            *converted = (int16_t)ival;
            return converted;
        }

        case FFI_TYPE_STRUCT: {
            // Accept only external pointer for struct field value
            if (TYPEOF(r_val) == EXTPTRSXP) {
                void* struct_ptr = R_ExternalPtrAddr(r_val);
                if (!struct_ptr) {
                    Rf_error("NULL external pointer for struct field");
                }
                // Return pointer to struct data
                return struct_ptr;
            } else {
                Rf_error("Cannot convert R value to native struct: must be external pointer");
            }
        }
        
        case FFI_TYPE_SINT32: {
            int32_t* converted = (int32_t*)R_alloc(1, sizeof(int32_t));
            if (TYPEOF(r_val) == INTSXP) {
                // Direct conversion - R integers are 32-bit signed
                *converted = INTEGER(r_val)[0];
                return converted;
            }
            if (!get_r_integer_value(r_val, &ival, &is_exact)) {
                Rf_error("Cannot convert to int32: unsupported R type");
            }
            // C99 modular conversion
            *converted = (int32_t)ival;
            return converted;
        }
            
        case FFI_TYPE_SINT64: {
            int64_t* converted = (int64_t*)R_alloc(1, sizeof(int64_t));
            if (!get_r_integer_value(r_val, &ival, &is_exact)) {
                Rf_error("Cannot convert to int64: unsupported R type");
            }
            *converted = ival;
            return converted;
        }
            
        case FFI_TYPE_UINT8: {
            uint8_t* converted = (uint8_t*)R_alloc(1, sizeof(uint8_t));
            if (!get_r_unsigned_value(r_val, &uval, &is_exact)) {
                Rf_error("Cannot convert to uint8: unsupported R type");
            }
            // C99: modular arithmetic for unsigned (value mod 256)
            *converted = (uint8_t)uval;
            return converted;
        }
            
        case FFI_TYPE_UINT16: {
            uint16_t* converted = (uint16_t*)R_alloc(1, sizeof(uint16_t));
            if (!get_r_unsigned_value(r_val, &uval, &is_exact)) {
                Rf_error("Cannot convert to uint16: unsupported R type");
            }
            // C99: modular arithmetic for unsigned (value mod 65536)
            *converted = (uint16_t)uval;
            return converted;
        }
            
        case FFI_TYPE_UINT32: {
            uint32_t* converted = (uint32_t*)R_alloc(1, sizeof(uint32_t));
            if (!get_r_unsigned_value(r_val, &uval, &is_exact)) {
                Rf_error("Cannot convert to uint32: unsupported R type");
            }
            // C99: modular arithmetic for unsigned
            *converted = (uint32_t)uval;
            return converted;
        }
            
        case FFI_TYPE_UINT64: {
            uint64_t* converted = (uint64_t*)R_alloc(1, sizeof(uint64_t));
            if (!get_r_unsigned_value(r_val, &uval, &is_exact)) {
                Rf_error("Cannot convert to uint64: unsupported R type");
            }
            *converted = uval;
            return converted;
        }
            
        #if FFI_TYPE_LONGDOUBLE != FFI_TYPE_DOUBLE
        case FFI_TYPE_LONGDOUBLE: {
            long double* converted = (long double*)R_alloc(1, sizeof(long double));
            if (!get_r_double_value(r_val, &dval)) {
                Rf_error("Cannot convert to long double: unsupported R type");
            }
            *converted = (long double)dval;
            return converted;
        }
        #endif
            
        default:
            // Custom types are handled above; if we get here, it's unsupported
            Rf_error("Unsupported FFI type for conversion: %d", type->type);
            
        case FFI_TYPE_DOUBLE: {
            double* converted = (double*)R_alloc(1, sizeof(double));
            if (!get_r_double_value(r_val, &dval)) {
                Rf_error("Cannot convert to double: unsupported R type");
            }
            *converted = dval;
            return converted;
        }
            
        case FFI_TYPE_FLOAT: {
            float* converted = (float*)R_alloc(1, sizeof(float));
            if (!get_r_double_value(r_val, &dval)) {
                Rf_error("Cannot convert to float: unsupported R type");
            }
            // C99: double to float conversion may lose precision
            *converted = (float)dval;
            return converted;
        }
        
        case FFI_TYPE_POINTER:
            // Handle custom string type separately
            if (type == &ffi_type_string_custom) {
                if (TYPEOF(r_val) == STRSXP && LENGTH(r_val) > 0) {
                   const char* str = CHAR(STRING_ELT(r_val, 0));
                    const char** ptr = (const char**)R_alloc(1, sizeof(const char*));
                    *ptr = str;
                    return ptr;
                } else if (r_val == R_NilValue) {
                    static const char* null_str = NULL;
                    return &null_str;
                } else {
                    Rf_error("Cannot convert to string (expected character vector or NULL)");
                }
            }
            // Handle generic pointers
            if (TYPEOF(r_val) == EXTPTRSXP) {
                void* addr = R_ExternalPtrAddr(r_val);
                // we will let this leak
                void** ptr = (void**)R_alloc(1, sizeof(void*));
                *ptr = addr;
                return ptr;
            } else if (TYPEOF(r_val) == STRSXP && LENGTH(r_val) > 0) {
                 const char* str = CHAR(STRING_ELT(r_val, 0));
                    const char** ptr = (const char**)R_alloc(1, sizeof(const char*));
                    *ptr = str;
                    return ptr;
            } else if (r_val == R_NilValue) {
                static void* null_ptr = NULL;
                return &null_ptr;
            } else {
                Rf_error("Cannot convert to pointer (supported: external pointer, character string, NULL)");
            }
    }
}

// Convert native return value to R value
// 
// Type conversion strategy (native to R):
// - Signed integers that fit in R's integer range: return as integer
// - Unsigned integers: uint8/uint16 fit in integer, uint32/uint64 use double
// - int64: uses double (may lose precision for values > 2^53)
// - Floating point: all return as R double
// - Pointers: return as external pointer
SEXP convert_native_to_r(void* value, ffi_type* type) {
    // Handle custom types first (before switch on type->type)
    // These custom types may share type codes with standard types
    if (type == &ffi_type_string_custom) {
        char* str = *(char**)value;
        if (str == NULL) {
            return R_NilValue;
        } else {
            return mkString(str);
        }
    } else if (type == &ffi_type_size_t_custom) {
        size_t val = *(size_t*)value;
        // size_t is unsigned, may exceed INT_MAX
        if (val <= INT_MAX) {
            return ScalarInteger((int)val);
        }
        return ScalarReal((double)val);
    } else if (type == &ffi_type_ssize_t_custom) {
        ssize_t val = *(ssize_t*)value;
        // ssize_t is signed, check if fits in R integer
        if (val >= INT_MIN && val <= INT_MAX) {
            return ScalarInteger((int)val);
        }
        return ScalarReal((double)val);
    } else if (type == &ffi_type_bool_custom) {
        // C99 _Bool returns as R logical
        return ScalarLogical(*(_Bool*)value != 0);
    } else if (type == &ffi_type_wchar_t_custom) {
        wchar_t wc = *(wchar_t*)value;
        // Return as integer - user can convert if needed
        return ScalarInteger((int)wc);
    } else if (type == &ffi_type_raw_custom) {
        // raw type returns as integer (single byte)
        return ScalarInteger((int)(*(uint8_t*)value));
    }
    
    // Now handle standard libffi types
    switch (type->type) {
        case FFI_TYPE_VOID:
            return R_NilValue;
            
        case FFI_TYPE_SINT8:
            // int8 always fits in R integer
            return ScalarInteger((int)(*(int8_t*)value));
            
        case FFI_TYPE_SINT16:
            // int16 always fits in R integer
            return ScalarInteger((int)(*(int16_t*)value));
            
        case FFI_TYPE_SINT32:
            // int32 = R integer (same type)
            return ScalarInteger(*(int32_t*)value);
            
        case FFI_TYPE_SINT64: {
            // int64 may not fit in R integer, use double
            // Note: precision loss for |value| > 2^53
            int64_t val = *(int64_t*)value;
            return ScalarReal((double)val);
        }
            
        case FFI_TYPE_UINT8:
            // uint8 [0, 255] always fits in R integer
            return ScalarInteger((int)(*(uint8_t*)value));
            
        case FFI_TYPE_UINT16:
            // uint16 [0, 65535] always fits in R integer
            return ScalarInteger((int)(*(uint16_t*)value));
            
        case FFI_TYPE_UINT32: {
            // uint32 [0, 4294967295] may not fit in R integer (max ~2.1B)
            // Values > INT_MAX are returned as double
            uint32_t val = *(uint32_t*)value;
            if (val <= INT_MAX) {
                return ScalarInteger((int)val);
            }
            return ScalarReal((double)val);
        }
            
        case FFI_TYPE_UINT64: {
            // uint64 - use double (may lose precision for values > 2^53)
            uint64_t val = *(uint64_t*)value;
            return ScalarReal((double)val);
        }
            
        case FFI_TYPE_DOUBLE:
            return ScalarReal(*(double*)value);
            
        case FFI_TYPE_FLOAT:
            // float promotes to double
            return ScalarReal((double)(*(float*)value));
            
        #if FFI_TYPE_LONGDOUBLE != FFI_TYPE_DOUBLE
        case FFI_TYPE_LONGDOUBLE:
            // long double demotes to double (may lose precision)
            return ScalarReal((double)(*(long double*)value));
        #endif
            
        case FFI_TYPE_POINTER: {
            void* ptr = *(void**)value;
            if (ptr == NULL) {
                return R_NilValue;
            }
            // Always return ExternalPtr for type safety - no dangerous heuristics
            // Users should use explicit string conversion functions when needed
            return R_MakeExternalPtr(ptr, Rf_install("generic_pointer"), R_NilValue);
        }

        // TODO : structs should be handled more reliably
        // we could build on the the fly SStructType objects
        // by going through the elements of ffi_type->elements recursively
        case FFI_TYPE_STRUCT: {
            void* ptr = value;
            if (ptr == NULL) {
                return R_NilValue;
            }
            return R_MakeExternalPtr(ptr, Rf_install("struct_pointer"), R_NilValue);
        }
        
        default:
            Rf_error("Unsupported FFI type for return conversion: %d", type->type);
    }
}


/*
**
**
** FFI function calls
****

*/


// Context structure for cleanup on error
typedef struct {
    int protected_count;
} ffi_call_context_t;

// Cleanup function for UNWIND_PROTECT
static void ffi_call_cleanup(void* data, Rboolean jump) {
    ffi_call_context_t* ctx = (ffi_call_context_t*)data;
    if (ctx->protected_count > 0) {
        UNPROTECT(ctx->protected_count);
    }
}

// Prepare FFI call interface (CIF)
SEXP R_prep_ffi_cif(SEXP r_return_type, SEXP r_arg_types) {
    ffi_type* return_type = (ffi_type*)R_ExternalPtrAddr(r_return_type);
    if (!return_type) {
        Rf_error("Invalid return type");
    }
    
    int num_args = LENGTH(r_arg_types);
    ffi_type** arg_types = NULL;
    
    if (num_args > 0) {
        arg_types = (ffi_type**)malloc(sizeof(ffi_type*) * num_args);
        if (!arg_types) {
            Rf_error("Memory allocation failed");
        }
        
        for (int i = 0; i < num_args; i++) {
            SEXP arg_type = VECTOR_ELT(r_arg_types, i);
            arg_types[i] = (ffi_type*)R_ExternalPtrAddr(arg_type);
            if (!arg_types[i]) {
                // previous allocation may leek here
                // we are not sure here because arg_types_i may be reused
                // and is normally managed by R                
                free(arg_types);
                Rf_error("Invalid argument type at index %d", i + 1);
            }
        }
    }
    
    // Allocate and prepare CIF
    ffi_cif* cif = (ffi_cif*)malloc(sizeof(ffi_cif));
    if (!cif) {
        if (arg_types) free(arg_types);
        Rf_error("Memory allocation failed");
    }
    
    ffi_status status = ffi_prep_cif(cif, FFI_DEFAULT_ABI, num_args, return_type, arg_types);
    if (status != FFI_OK) {
        free(cif);
        if (arg_types) free(arg_types);
        Rf_error("Failed to prepare FFI call interface (status: %d)", status);
    }
    // TO DO add tags to the cif for the return type and arg type converstions
    return R_MakeExternalPtr(cif, R_NilValue, R_NilValue);
}

// Prepare FFI call interface for variadic functions
// nfixedargs: number of fixed arguments (before the ...)
// r_arg_types: list of ALL argument types (fixed + variadic)
SEXP R_prep_ffi_cif_var(SEXP r_return_type, SEXP r_arg_types, SEXP r_nfixedargs) {
    ffi_type* return_type = (ffi_type*)R_ExternalPtrAddr(r_return_type);
    if (!return_type) {
        Rf_error("Invalid return type");
    }
    
    int nfixedargs = asInteger(r_nfixedargs);
    int ntotalargs = LENGTH(r_arg_types);
    
    if (nfixedargs < 0 || nfixedargs > ntotalargs) {
        Rf_error("nfixedargs must be between 0 and total number of arguments");
    }
    
    ffi_type** arg_types = NULL;
    
    if (ntotalargs > 0) {
        arg_types = (ffi_type**)malloc(sizeof(ffi_type*) * ntotalargs);
        if (!arg_types) {
            Rf_error("Memory allocation failed");
        }
        
        for (int i = 0; i < ntotalargs; i++) {
            SEXP arg_type = VECTOR_ELT(r_arg_types, i);
            arg_types[i] = (ffi_type*)R_ExternalPtrAddr(arg_type);
            if (!arg_types[i]) {
                free(arg_types);
                Rf_error("Invalid argument type at index %d", i + 1);
            }
        }
    }
    
    ffi_cif* cif = (ffi_cif*)malloc(sizeof(ffi_cif));
    if (!cif) {
        if (arg_types) free(arg_types);
        Rf_error("Memory allocation failed");
    }
    
    // Use ffi_prep_cif_var for variadic functions
    ffi_status status = ffi_prep_cif_var(cif, FFI_DEFAULT_ABI, 
                                          nfixedargs, ntotalargs, 
                                          return_type, arg_types);
    if (status != FFI_OK) {
        free(cif);
        if (arg_types) free(arg_types);
        if (status == FFI_BAD_ARGTYPE) {
            Rf_error("FFI_BAD_ARGTYPE: variadic args cannot be float or small integers (use int/double)");
        }
        Rf_error("Failed to prepare variadic FFI call interface (status: %d)", status);
    }
    
    return R_MakeExternalPtr(cif, Rf_install("ffi_cif_var"), R_NilValue);
}


// Internal function that does the actual work
static SEXP do_ffi_call_internal(void* data) {
    SEXP* args = (SEXP*)data;
    SEXP r_cif = args[0];
    SEXP r_func_ptr = args[1];
    SEXP r_args = args[2];
    int na_check = asLogical(args[3]);
    
    // Validate external pointer types
    if (TYPEOF(r_cif) != EXTPTRSXP) {
        Rf_error("CIF must be an external pointer (got type %d)", TYPEOF(r_cif));
    }
    if (TYPEOF(r_func_ptr) != EXTPTRSXP) {
        Rf_error("Function pointer must be an external pointer (got type %d)", TYPEOF(r_func_ptr));
    }
    
    ffi_cif* cif = (ffi_cif*)R_ExternalPtrAddr(r_cif);
    void* func_ptr = R_ExternalPtrAddr(r_func_ptr);
    
    // Validate pointers - these are the checks that can prevent crashes
    if (!cif) {
        Rf_error("Invalid CIF pointer (NULL). The CIF object may have been freed or corrupted.");
    }
    if (!func_ptr) {
        Rf_error("Invalid function pointer (NULL). The symbol may not exist or the library may have been unloaded.");
    }
    
    // Validate CIF structure looks reasonable
    // Note: We can't fully validate the CIF, but we can check some basics
    if (cif->nargs > 1000) {
        // Sanity check - no reasonable function has more than 1000 args
        Rf_error("CIF appears corrupted: nargs=%u is unreasonably large", cif->nargs);
    }
    if (cif->rtype == NULL) {
        Rf_error("CIF appears corrupted: return type is NULL");
    }
    
    int num_args = LENGTH(r_args);
    if (num_args != (int)cif->nargs) {
        Rf_error("Argument count mismatch: expected %d, got %d", (int)cif->nargs, num_args);
    }
    
    // Validate arg_types array if there are arguments
    if (num_args > 0 && cif->arg_types == NULL) {
        Rf_error("CIF appears corrupted: arg_types is NULL but nargs=%d", num_args);
    }
    
    // Check for NA values if requested
    if (na_check) {
        for (int i = 0; i < num_args; i++) {
            SEXP arg = VECTOR_ELT(r_args, i);
            if (has_na_value(arg)) {
                Rf_error("NA value not allowed in argument %d. Use na_check=FALSE to allow (at your own risk).", i + 1);
            }
        }
    }
    
    // Set up cleanup context
    ffi_call_context_t ctx = {0};
    
    // PROTECT r_args first
    PROTECT(r_args);
    ctx.protected_count = 1;
    
    void** arg_values = NULL;
    if (num_args > 0) {
        // Validate each arg_type pointer before conversion
        for (int i = 0; i < num_args; i++) {
            if (cif->arg_types[i] == NULL) {
                UNPROTECT(1);  // r_args
                Rf_error("CIF appears corrupted: arg_types[%d] is NULL", i);
            }
        }
        
        // Use R_alloc for automatic cleanup - freed when .Call() returns
        arg_values = (void**)R_alloc(num_args, sizeof(void*));
        
        // Protect each individual argument and convert - with error safety
        for (int i = 0; i < num_args; i++) {
            SEXP arg = VECTOR_ELT(r_args, i);
            PROTECT(arg);  
            ctx.protected_count++;  // Track protections for cleanup
            
            // This can error - but cleanup will handle unprotecting
            arg_values[i] = convert_r_to_native(arg, cif->arg_types[i]);
           // Rprintf("Converted argument %d to native value at %p\n", i, arg_values[i]);
        }
    }
    
    // Allocate space for return value using R_alloc - automatic cleanup
    void* return_value = NULL;
    if (cif->rtype->type != FFI_TYPE_VOID) {
        return_value = R_alloc(1, cif->rtype->size);
    }
    
    // Make the FFI call - all R objects are now protected
    typedef void (*generic_func_ptr)();
    union { void *obj; generic_func_ptr func; } caster;
    caster.obj = func_ptr;
    ffi_call(cif, caster.func, return_value, arg_values);
    
    // Convert return value - this can also error
    SEXP result = R_NilValue;
    if (return_value) {
        result = convert_native_to_r(return_value, cif->rtype);
    }
    
    // Success - manually unprotect before returning
    UNPROTECT(ctx.protected_count);
    ctx.protected_count = 0;  // Mark as cleaned up
    
    return result;
}

// Make FFI function call - SAFE VERSION with balanced PROTECT/UNPROTECT
// na_check: if TRUE, check for NA values and error if found
SEXP R_ffi_call(SEXP r_cif, SEXP r_func_ptr, SEXP r_args, SEXP r_na_check) {
    // Use UNWIND_PROTECT for automatic cleanup on error
    SEXP args[4] = {r_cif, r_func_ptr, r_args, r_na_check};
    ffi_call_context_t ctx = {0};
    
    return R_UnwindProtect(do_ffi_call_internal, args, ffi_call_cleanup, &ctx, NULL);
}




/*
*
*
* MISC FUNCTIONS
*
*/


// Allocate structure memory
SEXP R_alloc_struct(SEXP r_struct_type) {
    ffi_type* struct_type = (ffi_type*)R_ExternalPtrAddr(r_struct_type);
    if (!struct_type || struct_type->type != FFI_TYPE_STRUCT) {
        Rf_error("Invalid structure type");
    }
    
    void* ptr = calloc(1, struct_type->size);
    if (!ptr) {
        Rf_error("Memory allocation failed");
    }
    
    SEXP extPtr = R_MakeExternalPtr(ptr, R_NilValue, R_NilValue);
    PROTECT(extPtr);
    R_RegisterCFinalizerEx(extPtr, buffer_finalizer, TRUE);
    UNPROTECT(1);
    return extPtr;
}

// Simple offset calculation for struct fields (natural alignment)
static size_t calculate_field_offset(ffi_type* struct_type, int field_index) {
    if (!struct_type->elements || field_index < 0) return 0;
    
    size_t offset = 0;
    for (int i = 0; i < field_index && struct_type->elements[i]; i++) {
        ffi_type* field_type = struct_type->elements[i];
        
        // Align to field boundary
        size_t alignment = field_type->alignment;
        if (alignment > 0) {
            offset = (offset + alignment - 1) & ~(alignment - 1);
        }
        
        offset += field_type->size;
    }
    
    // Final alignment for the target field
    if (struct_type->elements[field_index]) {
        size_t alignment = struct_type->elements[field_index]->alignment;
        if (alignment > 0) {
            offset = (offset + alignment - 1) & ~(alignment - 1);
        }
    }
    
    return offset;
}

// Packed offset calculation - respects pack parameter like #pragma pack(n)
// pack: alignment limit (1, 2, 4, 8, 16). 0 means natural alignment.
static size_t calculate_packed_field_offset(ffi_type* struct_type, int field_index, int pack) {
    if (!struct_type->elements || field_index < 0) return 0;
    if (pack <= 0) return calculate_field_offset(struct_type, field_index);
    
    size_t offset = 0;
    for (int i = 0; i < field_index && struct_type->elements[i]; i++) {
        ffi_type* field_type = struct_type->elements[i];
        
        // Effective alignment is min(natural_alignment, pack)
        size_t natural_align = field_type->alignment;
        size_t effective_align = (natural_align < (size_t)pack) ? natural_align : (size_t)pack;
        
        // Align to effective boundary
        if (effective_align > 0) {
            offset = (offset + effective_align - 1) & ~(effective_align - 1);
        }
        
        offset += field_type->size;
    }
    
    // Final alignment for the target field
    if (struct_type->elements[field_index]) {
        size_t natural_align = struct_type->elements[field_index]->alignment;
        size_t effective_align = (natural_align < (size_t)pack) ? natural_align : (size_t)pack;
        if (effective_align > 0) {
            offset = (offset + effective_align - 1) & ~(effective_align - 1);
        }
    }
    
    return offset;
}

// Calculate packed struct size (for arrays of packed structs)
static size_t calculate_packed_struct_size(ffi_type* struct_type, int pack) {
    if (!struct_type->elements) return 0;
    if (pack <= 0) return struct_type->size;
    
    size_t offset = 0;
    size_t max_align = 1;
    
    for (int i = 0; struct_type->elements[i]; i++) {
        ffi_type* field_type = struct_type->elements[i];
        
        size_t natural_align = field_type->alignment;
        size_t effective_align = (natural_align < (size_t)pack) ? natural_align : (size_t)pack;
        if (effective_align > max_align) max_align = effective_align;
        
        // Align to effective boundary
        if (effective_align > 0) {
            offset = (offset + effective_align - 1) & ~(effective_align - 1);
        }
        
        offset += field_type->size;
    }
    
    // Struct alignment is min(max field alignment, pack) for trailing padding
    size_t struct_align = (max_align < (size_t)pack) ? max_align : (size_t)pack;
    if (struct_align > 0) {
        offset = (offset + struct_align - 1) & ~(struct_align - 1);
    }
    
    return offset;
}

// R interface to get packed field offset
SEXP R_get_packed_field_offset(SEXP r_struct_type, SEXP r_field_index, SEXP r_pack) {
    ffi_type* struct_type = (ffi_type*)R_ExternalPtrAddr(r_struct_type);
    int field_index = INTEGER(r_field_index)[0];
    int pack = INTEGER(r_pack)[0];
    
    if (!struct_type || struct_type->type != FFI_TYPE_STRUCT) {
        Rf_error("Invalid structure type");
    }
    
    size_t offset = calculate_packed_field_offset(struct_type, field_index, pack);
    return ScalarInteger((int)offset);
}

// R interface to get all packed field offsets
SEXP R_get_all_packed_field_offsets(SEXP r_struct_type, SEXP r_pack) {
    ffi_type* struct_type = (ffi_type*)R_ExternalPtrAddr(r_struct_type);
    int pack = INTEGER(r_pack)[0];
    
    if (!struct_type || struct_type->type != FFI_TYPE_STRUCT) {
        Rf_error("Invalid structure type");
    }
    
    // Count fields
    int num_fields = 0;
    while (struct_type->elements[num_fields]) num_fields++;
    
    SEXP result = PROTECT(allocVector(INTSXP, num_fields));
    int* offsets = INTEGER(result);
    
    for (int i = 0; i < num_fields; i++) {
        offsets[i] = (int)calculate_packed_field_offset(struct_type, i, pack);
    }
    
    UNPROTECT(1);
    return result;
}

// R interface to get packed struct size
SEXP R_get_packed_struct_size(SEXP r_struct_type, SEXP r_pack) {
    ffi_type* struct_type = (ffi_type*)R_ExternalPtrAddr(r_struct_type);
    int pack = INTEGER(r_pack)[0];
    
    if (!struct_type || struct_type->type != FFI_TYPE_STRUCT) {
        Rf_error("Invalid structure type");
    }
    
    size_t size = calculate_packed_struct_size(struct_type, pack);
    return ScalarInteger((int)size);
}

// R interface to get libffi's internal struct offsets (for debugging)
SEXP R_get_libffi_struct_offsets(SEXP r_struct_type) {
    ffi_type* struct_type = (ffi_type*)R_ExternalPtrAddr(r_struct_type);
    
    if (!struct_type || struct_type->type != FFI_TYPE_STRUCT) {
        Rf_error("Invalid structure type");
    }
    
    // Count fields
    int num_fields = 0;
    while (struct_type->elements[num_fields]) num_fields++;
    
    // Allocate array for offsets
    size_t* raw_offsets = (size_t*)R_alloc(num_fields, sizeof(size_t));
    
    // Call libffi's offset calculation
    ffi_status status = ffi_get_struct_offsets(FFI_DEFAULT_ABI, struct_type, raw_offsets);
    if (status != FFI_OK) {
        Rf_error("ffi_get_struct_offsets failed with status %d", status);
    }
    
    SEXP result = PROTECT(allocVector(INTSXP, num_fields));
    int* result_offsets = INTEGER(result);
    for (int i = 0; i < num_fields; i++) {
        result_offsets[i] = (int)raw_offsets[i];
    }
    
    UNPROTECT(1);
    return result;
}

// Get structure field value
SEXP R_get_struct_field(SEXP r_struct_ptr, SEXP r_field_index, SEXP r_struct_type) {
    void* struct_ptr = R_ExternalPtrAddr(r_struct_ptr);
    ffi_type* struct_type = (ffi_type*)R_ExternalPtrAddr(r_struct_type);
    int field_index = INTEGER(r_field_index)[0];
    
    if (!struct_ptr) Rf_error("Invalid structure pointer");
    if (!struct_type || struct_type->type != FFI_TYPE_STRUCT) Rf_error("Invalid structure type");
    
    if (!struct_type->elements || !struct_type->elements[field_index]) {
        Rf_error("Field index out of range: %d", field_index);
    }
    
    ffi_type* field_type = struct_type->elements[field_index];
    size_t offset = calculate_field_offset(struct_type, field_index);
    void* field_ptr = (char*)struct_ptr + offset;
    
    return convert_native_to_r(field_ptr, field_type);
}

// Set structure field value  
SEXP R_set_struct_field(SEXP r_struct_ptr, SEXP r_field_index, SEXP r_value, SEXP r_struct_type) {
    void* struct_ptr = R_ExternalPtrAddr(r_struct_ptr);
    ffi_type* struct_type = (ffi_type*)R_ExternalPtrAddr(r_struct_type);
    int field_index = INTEGER(r_field_index)[0];
    
    if (!struct_ptr) Rf_error("Invalid structure pointer");
    if (!struct_type || struct_type->type != FFI_TYPE_STRUCT) Rf_error("Invalid structure type");
    
    if (!struct_type->elements || !struct_type->elements[field_index]) {
        Rf_error("Field index out of range: %d", field_index);
    }
    
    ffi_type* field_type = struct_type->elements[field_index];
    size_t offset = calculate_field_offset(struct_type, field_index);
    void* field_ptr = (char*)struct_ptr + offset;
    
    void* value_ptr = convert_r_to_native(r_value, field_type);
    memcpy(field_ptr, value_ptr, field_type->size);
    
    return R_NilValue;
}

// Get struct field value at explicit offset (for packed structs)
// This allows R to compute the offset for packed structs instead of using libffi's alignment
SEXP R_get_struct_field_at_offset(SEXP r_struct_ptr, SEXP r_offset, SEXP r_field_type) {
    void* struct_ptr = R_ExternalPtrAddr(r_struct_ptr);
    int offset = INTEGER(r_offset)[0];
    ffi_type* field_type = (ffi_type*)R_ExternalPtrAddr(r_field_type);
    
    if (!struct_ptr) Rf_error("Invalid structure pointer");
    if (!field_type) Rf_error("Invalid field type");
    if (offset < 0) Rf_error("Invalid offset: %d", offset);
    
    void* field_ptr = (char*)struct_ptr + offset;
    return convert_native_to_r(field_ptr, field_type);
}

// Set struct field value at explicit offset (for packed structs)
SEXP R_set_struct_field_at_offset(SEXP r_struct_ptr, SEXP r_offset, SEXP r_value, SEXP r_field_type) {
    void* struct_ptr = R_ExternalPtrAddr(r_struct_ptr);
    int offset = INTEGER(r_offset)[0];
    ffi_type* field_type = (ffi_type*)R_ExternalPtrAddr(r_field_type);
    
    if (!struct_ptr) Rf_error("Invalid structure pointer");
    if (!field_type) Rf_error("Invalid field type");
    if (offset < 0) Rf_error("Invalid offset: %d", offset);
    
    void* field_ptr = (char*)struct_ptr + offset;
    void* value_ptr = convert_r_to_native(r_value, field_type);
    memcpy(field_ptr, value_ptr, field_type->size);
    
    return R_NilValue;
}

// Get union field value
// Unions have all fields at offset 0, but we need to interpret via the correct type
SEXP R_get_union_field(SEXP r_union_ptr, SEXP r_field_type) {
    void* union_ptr = R_ExternalPtrAddr(r_union_ptr);
    ffi_type* field_type = (ffi_type*)R_ExternalPtrAddr(r_field_type);
    
    if (!union_ptr) Rf_error("Invalid union pointer");
    if (!field_type) Rf_error("Invalid field type");
    
    // All union fields are at offset 0
    return convert_native_to_r(union_ptr, field_type);
}

// Set union field value
// Unions have all fields at offset 0, but we need to write with the correct type
SEXP R_set_union_field(SEXP r_union_ptr, SEXP r_value, SEXP r_field_type) {
    void* union_ptr = R_ExternalPtrAddr(r_union_ptr);
    ffi_type* field_type = (ffi_type*)R_ExternalPtrAddr(r_field_type);
    
    if (!union_ptr) Rf_error("Invalid union pointer");
    if (!field_type) Rf_error("Invalid field type");
    
    // Convert R value to native
    void* value_ptr = convert_r_to_native(r_value, field_type);
    
    // All union fields are at offset 0, write to beginning
    memcpy(union_ptr, value_ptr, field_type->size);
    
    return R_NilValue;
}

// Check if pointer is NULL
SEXP R_is_null_pointer(SEXP r_ptr) {
    if (TYPEOF(r_ptr) != EXTPTRSXP) {
        return ScalarLogical(TRUE);
    }
    
    void* ptr = R_ExternalPtrAddr(r_ptr);
    return ScalarLogical(ptr == NULL);
}



// Copy array from native memory to R vector
SEXP R_copy_array(SEXP r_ptr, SEXP r_length, SEXP r_element_type) {
    void* ptr = R_ExternalPtrAddr(r_ptr);
    int length = INTEGER(r_length)[0];
    ffi_type* element_type = (ffi_type*)R_ExternalPtrAddr(r_element_type);
    
    if (!ptr) Rf_error("Invalid pointer");
    if (length <= 0) Rf_error("Invalid length");
    if (!element_type) Rf_error("Invalid element type");
    
    SEXP result;
    switch (element_type->type) {
        case FFI_TYPE_SINT32: {
            PROTECT(result = allocVector(INTSXP, length));
            int* src = (int*)ptr;
            int* dest = INTEGER(result);
            for (int i = 0; i < length; i++) {
                dest[i] = src[i];
            }
            UNPROTECT(1);
            break;
        }
        case FFI_TYPE_DOUBLE: {
            PROTECT(result = allocVector(REALSXP, length));
            double* src = (double*)ptr;
            double* dest = REAL(result);
            for (int i = 0; i < length; i++) {
                dest[i] = src[i];
            }
            UNPROTECT(1);
            break;
        }
        case FFI_TYPE_UINT8: {
            PROTECT(result = allocVector(RAWSXP, length));
            unsigned char* src = (unsigned char*)ptr;
            Rbyte* dest = RAW(result);
            for (int i = 0; i < length; i++) {
                dest[i] = src[i];
            }
            UNPROTECT(1);
            break;
        }
        default:
            Rf_error("Unsupported element type for array copy");
    }
    return result;
}


// ============================================================================
// ARRAY OF STRUCTS SUPPORT
// ============================================================================

// Allocate contiguous array of N structures
SEXP R_alloc_struct_array(SEXP r_struct_type, SEXP r_n) {
    ffi_type* struct_type = (ffi_type*)R_ExternalPtrAddr(r_struct_type);
    int n = asInteger(r_n);
    
    if (!struct_type || struct_type->type != FFI_TYPE_STRUCT) {
        Rf_error("Invalid structure type");
    }
    if (n <= 0) {
        Rf_error("n must be positive");
    }
    
    size_t total_size = (size_t)n * struct_type->size;
    void* ptr = calloc(1, total_size);
    if (!ptr) {
        Rf_error("Memory allocation failed for %d structs (%zu bytes)", n, total_size);
    }
    
    SEXP extPtr = R_MakeExternalPtr(ptr, Rf_install("struct_array"), R_NilValue);
    PROTECT(extPtr);
    R_RegisterCFinalizerEx(extPtr, buffer_finalizer, TRUE);
    UNPROTECT(1);
    return extPtr;
}

// Get pointer to element in struct array (0-based index from C, 1-based from R)
SEXP R_get_struct_array_element(SEXP r_ptr, SEXP r_index, SEXP r_struct_type) {
    void* base_ptr = R_ExternalPtrAddr(r_ptr);
    ffi_type* struct_type = (ffi_type*)R_ExternalPtrAddr(r_struct_type);
    int index = asInteger(r_index);
    
    if (!base_ptr) Rf_error("Invalid pointer");
    if (!struct_type || struct_type->type != FFI_TYPE_STRUCT) {
        Rf_error("Invalid structure type");
    }
    if (index < 0) Rf_error("Index must be non-negative");
    
    // Calculate offset to element
    void* element_ptr = (char*)base_ptr + (size_t)index * struct_type->size;
    
    // Return new external pointer to this element
    // NOTE: No finalizer - parent array owns memory
    return R_MakeExternalPtr(element_ptr, Rf_install("struct_element"), R_NilValue);
}

// Get struct size (useful for computing offsets)
SEXP R_get_struct_size(SEXP r_struct_type) {
    ffi_type* struct_type = (ffi_type*)R_ExternalPtrAddr(r_struct_type);
    if (!struct_type) Rf_error("Invalid structure type");
    return ScalarInteger((int)struct_type->size);
}

// Get number of fields in a struct
SEXP R_get_struct_num_fields(SEXP r_struct_type) {
    ffi_type* struct_type = (ffi_type*)R_ExternalPtrAddr(r_struct_type);
    if (!struct_type || !struct_type->elements) Rf_error("Invalid structure type");
    
    int count = 0;
    while (struct_type->elements[count]) count++;
    return ScalarInteger(count);
}

// Get field info: returns list(offset, size, alignment) for a field
SEXP R_get_field_info(SEXP r_struct_type, SEXP r_field_index) {
    ffi_type* struct_type = (ffi_type*)R_ExternalPtrAddr(r_struct_type);
    int field_index = INTEGER(r_field_index)[0];
    
    if (!struct_type || struct_type->type != FFI_TYPE_STRUCT) {
        Rf_error("Invalid structure type");
    }
    if (!struct_type->elements || !struct_type->elements[field_index]) {
        Rf_error("Field index out of range");
    }
    
    ffi_type* field_type = struct_type->elements[field_index];
    size_t offset = calculate_field_offset(struct_type, field_index);
    
    // Return as named list
    SEXP result = PROTECT(allocVector(VECSXP, 3));
    SEXP names = PROTECT(allocVector(STRSXP, 3));
    
    SET_VECTOR_ELT(result, 0, ScalarInteger((int)offset));
    SET_VECTOR_ELT(result, 1, ScalarInteger((int)field_type->size));
    SET_VECTOR_ELT(result, 2, ScalarInteger((int)field_type->alignment));
    
    SET_STRING_ELT(names, 0, mkChar("offset"));
    SET_STRING_ELT(names, 1, mkChar("size"));
    SET_STRING_ELT(names, 2, mkChar("alignment"));
    setAttrib(result, R_NamesSymbol, names);
    
    UNPROTECT(2);
    return result;
}

// Get all field offsets at once (more efficient than calling R_get_field_info repeatedly)
SEXP R_get_all_field_offsets(SEXP r_struct_type) {
    ffi_type* struct_type = (ffi_type*)R_ExternalPtrAddr(r_struct_type);
    
    if (!struct_type || struct_type->type != FFI_TYPE_STRUCT || !struct_type->elements) {
        Rf_error("Invalid structure type");
    }
    
    // Count fields
    int num_fields = 0;
    while (struct_type->elements[num_fields]) num_fields++;
    
    SEXP result = PROTECT(allocVector(INTSXP, num_fields));
    int* offsets = INTEGER(result);
    
    for (int i = 0; i < num_fields; i++) {
        offsets[i] = (int)calculate_field_offset(struct_type, i);
    }
    
    UNPROTECT(1);
    return result;
}


// Explicit pointer-to-string conversion for type safety
SEXP R_pointer_to_string(SEXP r_ptr) {
    if (TYPEOF(r_ptr) != EXTPTRSXP) {
        Rf_error("Expected external pointer");
    }
    
    char* str = (char*)R_ExternalPtrAddr(r_ptr);
    if (str == NULL) {
        return R_NilValue;
    }
    
    return mkString(str);
}

// Create typed external pointer with proper tag like Rffi
SEXP R_make_typed_pointer(SEXP r_ptr, SEXP r_type_name) {
    void* ptr = NULL;
    
    if (TYPEOF(r_ptr) == EXTPTRSXP) {
        ptr = R_ExternalPtrAddr(r_ptr);
    } else {
        Rf_error("Expected external pointer");
    }
    
    const char* type_name = CHAR(STRING_ELT(r_type_name, 0));
    
    return R_MakeExternalPtr(ptr, Rf_install(type_name), R_NilValue);
}



// Get libffi version
SEXP R_libffi_version() {
#ifdef FFI_VERSION_STRING
    return mkString(FFI_VERSION_STRING);
#else
    return mkString("unknown");
#endif
}

// Get the type tag of an external pointer

SEXP R_get_pointer_type(SEXP r_ptr) {
    if (TYPEOF(r_ptr) != EXTPTRSXP) {
        Rf_error("Expected external pointer");
    }
    
    SEXP tag = R_ExternalPtrTag(r_ptr);
    if (tag == R_NilValue) {
        return mkString("unknown");
    }
    
    return mkString(CHAR(PRINTNAME(tag)));
}

// Dereference a pointer - read the pointer value stored at an address
// This is useful for reading global variables that are pointers (like R_GlobalEnv)
SEXP R_deref_pointer(SEXP r_ptr) {
    if (TYPEOF(r_ptr) != EXTPTRSXP) {
        Rf_error("Expected external pointer");
    }
    
    void** ptr_addr = (void**)R_ExternalPtrAddr(r_ptr);
    if (ptr_addr == NULL) {
        return R_MakeExternalPtr(NULL, R_NilValue, R_NilValue);
    }
    
    // Read the pointer value at this address
    void* value = *ptr_addr;
    
    return R_MakeExternalPtr(value, R_NilValue, R_NilValue);
}

// Read a typed value from a global symbol address
// Supports: pointer, int, double, etc.
SEXP R_read_global(SEXP r_ptr, SEXP r_type) {
    if (TYPEOF(r_ptr) != EXTPTRSXP) {
        Rf_error("Expected external pointer for address");
    }
    if (TYPEOF(r_type) != EXTPTRSXP) {
        Rf_error("Expected external pointer for type");
    }
    
    void* addr = R_ExternalPtrAddr(r_ptr);
    if (addr == NULL) {
        Rf_error("NULL address");
    }
    
    ffi_type* type = (ffi_type*)R_ExternalPtrAddr(r_type);
    if (type == NULL) {
        Rf_error("Invalid type");
    }
    
    // Use convert_native_to_r to handle the conversion
    return convert_native_to_r(addr, type);
}


/*
*
*
* CLOSURE API
*
* Allows R functions to be used as C callbacks
*
*/

#ifdef FFI_CLOSURES

// Structure to hold closure data - stored with the closure
typedef struct {
    SEXP r_function;      // The R function to call
    SEXP r_cif;           // CIF external pointer (to prevent GC)
    ffi_cif* cif;         // Direct pointer to CIF for fast access
    void* executable;     // Executable address for the closure
} closure_data_t;

// Generic callback handler - called by libffi when closure is invoked
static void closure_callback_handler(ffi_cif* cif, void* ret, void** args, void* user_data) {
    closure_data_t* data = (closure_data_t*)user_data;
    
    if (!data || !data->r_function) {
        Rf_error("Invalid closure data");
    }
    
    // Build R argument list from native args
    int nargs = cif->nargs;
    SEXP call;
    PROTECT(call = allocVector(LANGSXP, nargs + 1));
    SETCAR(call, data->r_function);
    
    // Convert each native argument to R
    SEXP arg_cursor = CDR(call);
    for (int i = 0; i < nargs; i++) {
        SEXP r_arg = convert_native_to_r(args[i], cif->arg_types[i]);
        SETCAR(arg_cursor, r_arg);
        arg_cursor = CDR(arg_cursor);
    }
    
    // Call the R function
    int error_occurred = 0;
    SEXP result = R_tryEval(call, R_GlobalEnv, &error_occurred);
    
    if (error_occurred) {
        UNPROTECT(1);
        Rf_error("Error in R callback function");
    }
    
    PROTECT(result);
    
    // Convert result back to native if not void
    if (cif->rtype->type != FFI_TYPE_VOID && ret != NULL) {
        void* native_ret = convert_r_to_native(result, cif->rtype);
        memcpy(ret, native_ret, cif->rtype->size);
    }
    
    UNPROTECT(2);
}

// Closure finalizer - frees the closure when R GCs the external pointer
static void closure_finalizer(SEXP r_closure_ptr) {
    closure_data_t* data = (closure_data_t*)R_ExternalPtrAddr(r_closure_ptr);
    if (data) {
        // The closure memory (writable address) is what we free
        // data itself was allocated alongside or separately
        // ffi_closure_free expects the writable pointer
        ffi_closure_free(data);
    }
}

// Check if closures are supported
SEXP R_ffi_closures_supported() {
    return ScalarLogical(1);
}

// Create a closure from an R function
// Returns external pointer to closure that can be cast to function pointer
SEXP R_create_closure(SEXP r_function, SEXP r_cif) {
    if (!Rf_isFunction(r_function)) {
        Rf_error("First argument must be a function");
    }
    
    ffi_cif* cif = (ffi_cif*)R_ExternalPtrAddr(r_cif);
    if (!cif) {
        Rf_error("Invalid CIF pointer");
    }
    
    // Allocate closure with space for our data
    void* executable = NULL;
    ffi_closure* closure = ffi_closure_alloc(sizeof(ffi_closure) + sizeof(closure_data_t), &executable);
    
    if (!closure) {
        Rf_error("Failed to allocate closure memory");
    }
    
    // Our data lives right after the ffi_closure struct
    closure_data_t* data = (closure_data_t*)((char*)closure + sizeof(ffi_closure));
    data->r_function = r_function;
    data->r_cif = r_cif;
    data->cif = cif;
    data->executable = executable;
    
    // Prepare the closure
    ffi_status status = ffi_prep_closure_loc(
        closure,
        cif,
        closure_callback_handler,
        data,
        executable
    );
    
    if (status != FFI_OK) {
        ffi_closure_free(closure);
        Rf_error("Failed to prepare closure (status: %d)", status);
    }
    
    // Create external pointer for the closure (writable address for freeing)
    SEXP r_closure = PROTECT(R_MakeExternalPtr(closure, Rf_install("ffi_closure"), R_NilValue));
    R_RegisterCFinalizerEx(r_closure, closure_finalizer, TRUE);
    
    // Store the R function and CIF in the protected slot to prevent GC
    // The protected slot keeps these alive as long as the closure lives
    SEXP prot = PROTECT(allocVector(VECSXP, 2));
    SET_VECTOR_ELT(prot, 0, r_function);
    SET_VECTOR_ELT(prot, 1, r_cif);
    R_SetExternalPtrProtected(r_closure, prot);
    
    UNPROTECT(2);
    return r_closure;
}

// Get the executable pointer for a closure (to pass to C functions)
SEXP R_get_closure_pointer(SEXP r_closure) {
    ffi_closure* closure = (ffi_closure*)R_ExternalPtrAddr(r_closure);
    if (!closure) {
        Rf_error("Invalid closure pointer");
    }
    
    // Our data is stored right after the ffi_closure
    closure_data_t* data = (closure_data_t*)((char*)closure + sizeof(ffi_closure));
    
    // Return the executable address as an external pointer
    return R_MakeExternalPtr(data->executable, Rf_install("closure_func"), R_NilValue);
}

#else
// Closures not supported on this platform

SEXP R_ffi_closures_supported() {
    return ScalarLogical(0);
}

SEXP R_create_closure(SEXP r_function, SEXP r_cif) {
    Rf_error("FFI closures are not supported on this platform");
    return R_NilValue;
}

SEXP R_get_closure_pointer(SEXP r_closure) {
    Rf_error("FFI closures are not supported on this platform");
    return R_NilValue;
}

#endif /* FFI_CLOSURES */

/* ==========================================================================
 * 64-bit Bitfield Operations
 * 
 * R integers are 32-bit signed, which limits bitfield operations.
 * These C functions handle 64-bit bitfield operations using doubles
 * (which can exactly represent integers up to 2^53).
 * ==========================================================================*/

// Pack 64-bit bitfield: values and widths as integer vectors, returns double
SEXP R_ffi_pack_bits64(SEXP r_values, SEXP r_widths) {
    int n = LENGTH(r_values);
    if (n != LENGTH(r_widths)) {
        Rf_error("Length of values must match length of widths");
    }
    
    int* values = INTEGER(r_values);
    int* widths = INTEGER(r_widths);
    
    uint64_t result = 0;
    int bit_offset = 0;
    
    for (int i = 0; i < n; i++) {
        int width = widths[i];
        if (width <= 0 || width > 64) {
            Rf_error("Width must be between 1 and 64");
        }
        if (bit_offset + width > 64) {
            Rf_error("Total bit width exceeds 64 bits");
        }
        
        uint64_t mask = (width == 64) ? UINT64_MAX : ((1ULL << width) - 1);
        uint64_t val = ((uint64_t)values[i]) & mask;
        result |= (val << bit_offset);
        bit_offset += width;
    }
    
    return ScalarReal((double)result);
}

// Unpack 64-bit bitfield: packed_value as double, widths as integer vector
// Returns integer vector (values truncated to 32-bit)
SEXP R_ffi_unpack_bits64(SEXP r_packed, SEXP r_widths) {
    uint64_t packed = (uint64_t)REAL(r_packed)[0];
    int n = LENGTH(r_widths);
    int* widths = INTEGER(r_widths);
    
    SEXP result = PROTECT(allocVector(INTSXP, n));
    int* out = INTEGER(result);
    
    int bit_offset = 0;
    for (int i = 0; i < n; i++) {
        int width = widths[i];
        if (width <= 0 || width > 64) {
            UNPROTECT(1);
            Rf_error("Width must be between 1 and 64");
        }
        
        uint64_t mask = (width == 64) ? UINT64_MAX : ((1ULL << width) - 1);
        uint64_t val = (packed >> bit_offset) & mask;
        out[i] = (int)val;  // Truncate to 32-bit
        bit_offset += width;
    }
    
    UNPROTECT(1);
    return result;
}

// Extract single 64-bit bitfield
SEXP R_ffi_extract_bits64(SEXP r_packed, SEXP r_offset, SEXP r_width) {
    uint64_t packed = (uint64_t)REAL(r_packed)[0];
    int offset = INTEGER(r_offset)[0];
    int width = INTEGER(r_width)[0];
    
    if (width <= 0 || width > 64) {
        Rf_error("Width must be between 1 and 64");
    }
    if (offset < 0 || offset >= 64) {
        Rf_error("Offset must be between 0 and 63");
    }
    
    uint64_t mask = (width == 64) ? UINT64_MAX : ((1ULL << width) - 1);
    uint64_t val = (packed >> offset) & mask;
    
    return ScalarReal((double)val);
}

// Extract signed 64-bit bitfield (with sign extension)
SEXP R_ffi_extract_signed_bits64(SEXP r_packed, SEXP r_offset, SEXP r_width) {
    uint64_t packed = (uint64_t)REAL(r_packed)[0];
    int offset = INTEGER(r_offset)[0];
    int width = INTEGER(r_width)[0];
    
    if (width <= 0 || width > 64) {
        Rf_error("Width must be between 1 and 64");
    }
    if (offset < 0 || offset >= 64) {
        Rf_error("Offset must be between 0 and 63");
    }
    
    uint64_t mask = (width == 64) ? UINT64_MAX : ((1ULL << width) - 1);
    uint64_t val = (packed >> offset) & mask;
    
    // Sign extend if the high bit is set
    if (width < 64) {
        uint64_t sign_bit = 1ULL << (width - 1);
        if (val & sign_bit) {
            // Sign extend: set all bits above width
            val |= ~mask;
        }
    }
    
    // Return as signed integer (via double for range)
    int64_t signed_val = (int64_t)val;
    return ScalarReal((double)signed_val);
}

// Set single 64-bit bitfield
SEXP R_ffi_set_bits64(SEXP r_packed, SEXP r_value, SEXP r_offset, SEXP r_width) {
    uint64_t packed = (uint64_t)REAL(r_packed)[0];
    int64_t value = (int64_t)REAL(r_value)[0];
    int offset = INTEGER(r_offset)[0];
    int width = INTEGER(r_width)[0];
    
    if (width <= 0 || width > 64) {
        Rf_error("Width must be between 1 and 64");
    }
    if (offset < 0 || offset >= 64) {
        Rf_error("Offset must be between 0 and 63");
    }
    
    uint64_t mask = (width == 64) ? UINT64_MAX : ((1ULL << width) - 1);
    uint64_t clear_mask = ~(mask << offset);
    uint64_t set_val = ((uint64_t)value & mask) << offset;
    
    uint64_t result = (packed & clear_mask) | set_val;
    return ScalarReal((double)result);
}