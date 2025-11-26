#include <R.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>  // R's dynload facilities
#include "ffi.h"
#include <string.h>
#include <stdint.h>
#include <wchar.h>
#include <math.h>
#if !defined(FFI_VERSION_NUMBER) || FFI_VERSION_NUMBER < 30408
#error "libffi >= 3.4.8 is required"
#endif
// Type mapping structure
typedef struct {
    const char* name;
    ffi_type* type;
} ffi_type_map_t;

// Global table of built-in FFI types
// Custom FFI type objects for platform-dependent types
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

static ffi_type ffi_type_bool_custom = { 
    sizeof(int), 
    sizeof(int),  // alignment 
    FFI_TYPE_SINT32, 
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


// Finalizer for freeing malloc'd memory
static void buffer_finalizer(SEXP extPtr) {
    void* ptr = R_ExternalPtrAddr(extPtr);
    if (ptr) free(ptr);
    R_ClearExternalPtr(extPtr);
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


// Finalizer for freeing array ffi_type and its elements
static void array_type_finalizer(SEXP extPtr) {
    ffi_type* array_type = (ffi_type*)R_ExternalPtrAddr(extPtr);
    if (array_type) {
        if (array_type->elements) free(array_type->elements);
        free(array_type);
        R_ClearExternalPtr(extPtr);
    }
}
// Create array FFI type
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
    {"char", &ffi_type_sint8},           // c_char
    {"uchar", &ffi_type_uint8},          // c_ubyte  
    {"short", &ffi_type_sint16},         // c_short
    {"ushort", &ffi_type_uint16},        // c_ushort
    {"uint", &ffi_type_uint32},          // c_uint
    {"longlong", sizeof(long long) == 8 ? &ffi_type_sint64 : &ffi_type_sint32},  // c_longlong
    {"ulonglong", sizeof(unsigned long long) == 8 ? &ffi_type_uint64 : &ffi_type_uint32}, // c_ulonglong
    {NULL, NULL}
};



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


static void struct_type_finalizer(SEXP extPtr) {
    ffi_type* struct_type = (ffi_type*)R_ExternalPtrAddr(extPtr);
    if (struct_type) {
        if (struct_type->elements) free(struct_type->elements);
        free(struct_type);
        R_ClearExternalPtr(extPtr);
    }
}

// Create structure FFI type
SEXP R_create_struct_ffi_type(SEXP r_field_types) {
    int num_fields = LENGTH(r_field_types);
    if (num_fields == 0) {
        Rf_error("Structure must have at least one field");
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
    
   SEXP extPtr = R_MakeExternalPtr(struct_type, R_NilValue, R_NilValue);
    PROTECT(extPtr);
    R_RegisterCFinalizerEx(extPtr, struct_type_finalizer, TRUE);
    UNPROTECT(1);
    return extPtr;
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

// Convert R value to native value for FFI call
void* convert_r_to_native(SEXP r_val, ffi_type* type) {
    switch (type->type) {
        case FFI_TYPE_VOID:
            return NULL;
            
        case FFI_TYPE_SINT8: {
            int8_t* converted = (int8_t*)R_alloc(1, sizeof(int8_t));
            if (TYPEOF(r_val) == INTSXP) {
                int val = INTEGER(r_val)[0];
                if (val < -128 || val > 127) {
                    Rf_error("Integer %d out of range for int8 [-128, 127]", val);
                }
                *converted = (int8_t)val;
                return converted;
            } else if (TYPEOF(r_val) == REALSXP) {
                double val = REAL(r_val)[0];
                if (val < -128.0 || val > 127.0 || val != floor(val)) {
                    Rf_error("Value %g out of range or not integer for int8 [-128, 127]", val);
                }
                *converted = (int8_t)val;
                return converted;
            } else {
                Rf_error("Cannot convert to int8");
            }
        }
            
        case FFI_TYPE_SINT16: {
            int16_t* converted = (int16_t*)R_alloc(1, sizeof(int16_t));
            if (TYPEOF(r_val) == INTSXP) {
                int val = INTEGER(r_val)[0];
                if (val < -32768 || val > 32767) {
                    Rf_error("Integer %d out of range for int16 [-32768, 32767]", val);
                }
                *converted = (int16_t)val;
                return converted;
            } else if (TYPEOF(r_val) == REALSXP) {
                double val = REAL(r_val)[0];
                if (val < -32768.0 || val > 32767.0 || val != floor(val)) {
                    Rf_error("Value %g out of range or not integer for int16 [-32768, 32767]", val);
                }
                *converted = (int16_t)val;
                return converted;
            } else {
                Rf_error("Cannot convert to int16");
            }
        }

        case FFI_TYPE_SINT32:
            if (TYPEOF(r_val) == INTSXP) {
                // Direct pointer to R's integer data - safe with PROTECT
                return INTEGER(r_val);
            } else if (TYPEOF(r_val) == REALSXP) {
                double val = REAL(r_val)[0];
                if (val < -2147483648.0 || val > 2147483647.0 || val != floor(val)) {
                    Rf_error("Value %g out of range or not integer for int32 [-2147483648, 2147483647]", val);
                }
                int* converted = (int*)R_alloc(1, sizeof(int));
                *converted = (int)val;
                return converted;
            } else if (TYPEOF(r_val) == LGLSXP) {
                int* converted = (int*)R_alloc(1, sizeof(int));
                *converted = LOGICAL(r_val)[0];
                return converted;
            } else {
                Rf_error("Cannot convert to int");
            }
            
        case FFI_TYPE_SINT64: {
            int64_t* converted = (int64_t*)R_alloc(1, sizeof(int64_t));
            if (TYPEOF(r_val) == INTSXP) {
                *converted = (int64_t)INTEGER(r_val)[0];
                return converted;
            } else if (TYPEOF(r_val) == REALSXP) {
                double val = REAL(r_val)[0];
                // Note: double precision limits exact integer representation to ~53 bits
                if (val < -9007199254740992.0 || val > 9007199254740992.0 || val != floor(val)) {
                    Rf_error("Value %g out of range or not integer for safe int64 conversion", val);
                }
                *converted = (int64_t)val;
                return converted;
            } else {
                Rf_error("Cannot convert to int64");
            }
        }
            
        case FFI_TYPE_UINT8: {
            uint8_t* converted = (uint8_t*)R_alloc(1, sizeof(uint8_t));
            if (TYPEOF(r_val) == INTSXP) {
                int val = INTEGER(r_val)[0];
                if (val < 0 || val > 255) {
                    Rf_error("Integer %d out of range for uint8 [0, 255]", val);
                }
                *converted = (uint8_t)val;
                return converted;
            } else if (TYPEOF(r_val) == REALSXP) {
                double val = REAL(r_val)[0];
                if (val < 0.0 || val > 255.0 || val != floor(val)) {
                    Rf_error("Value %g out of range or not integer for uint8 [0, 255]", val);
                }
                *converted = (uint8_t)val;
                return converted;
            } else {
                Rf_error("Cannot convert to uint8");
            }
        }
            
        case FFI_TYPE_UINT16: {
            uint16_t* converted = (uint16_t*)R_alloc(1, sizeof(uint16_t));
            if (TYPEOF(r_val) == INTSXP) {
                int val = INTEGER(r_val)[0];
                if (val < 0 || val > 65535) {
                    Rf_error("Integer %d out of range for uint16 [0, 65535]", val);
                }
                *converted = (uint16_t)val;
                return converted;
            } else if (TYPEOF(r_val) == REALSXP) {
                double val = REAL(r_val)[0];
                if (val < 0.0 || val > 65535.0 || val != floor(val)) {
                    Rf_error("Value %g out of range or not integer for uint16 [0, 65535]", val);
                }
                *converted = (uint16_t)val;
                return converted;
            } else {
                Rf_error("Cannot convert to uint16");
            }
        }
            
        case FFI_TYPE_UINT32: {
            uint32_t* converted = (uint32_t*)R_alloc(1, sizeof(uint32_t));
            if (TYPEOF(r_val) == INTSXP) {
                int val = INTEGER(r_val)[0];
                if (val < 0) {
                    Rf_error("Integer %d out of range for uint32 [0, 4294967295]", val);
                }
                *converted = (uint32_t)val;
                return converted;
            } else if (TYPEOF(r_val) == REALSXP) {
                double val = REAL(r_val)[0];
                if (val < 0.0 || val > 4294967295.0 || val != floor(val)) {
                    Rf_error("Value %g out of range or not integer for uint32 [0, 4294967295]", val);
                }
                *converted = (uint32_t)val;
                return converted;
            } else {
                Rf_error("Cannot convert to uint32");
            }
        }
            
        case FFI_TYPE_UINT64: {
            uint64_t* converted = (uint64_t*)R_alloc(1, sizeof(uint64_t));
            if (TYPEOF(r_val) == INTSXP) {
                int val = INTEGER(r_val)[0];
                if (val < 0) {
                    Rf_error("Integer %d out of range for uint64 [0, 18446744073709551615]", val);
                }
                *converted = (uint64_t)val;
                return converted;
            } else if (TYPEOF(r_val) == REALSXP) {
                double val = REAL(r_val)[0];
                if (val < 0.0 || val > 18446744073709551615.0 || val != floor(val)) {
                    Rf_error("Value %g out of range or not integer for uint64", val);
                }
                *converted = (uint64_t)val;
                return converted;
            } else {
                Rf_error("Cannot convert to uint64");
            }
        }
            

            
        #if FFI_TYPE_LONGDOUBLE != FFI_TYPE_DOUBLE
        case FFI_TYPE_LONGDOUBLE: {
            if (TYPEOF(r_val) == REALSXP) {
                long double* converted = (long double*)R_alloc(1, sizeof(long double));
                *converted = (long double)REAL(r_val)[0];
                return converted;
            } else if (TYPEOF(r_val) == INTSXP) {
                long double* converted = (long double*)R_alloc(1, sizeof(long double));
                *converted = (long double)INTEGER(r_val)[0];
                return converted;
            } else {
                Rf_error("Cannot convert to long double");
            }
        }
        #endif
            
        // Handle custom platform-dependent types by their underlying type
        default:
            // Check if this is one of our custom types and handle accordingly
            if (type == &ffi_type_size_t_custom) {
                if (TYPEOF(r_val) == INTSXP) {
                    size_t* converted = (size_t*)R_alloc(1, sizeof(size_t));
                    *converted = (size_t)INTEGER(r_val)[0];
                    return converted;
                } else if (TYPEOF(r_val) == REALSXP) {
                    size_t* converted = (size_t*)R_alloc(1, sizeof(size_t));
                    *converted = (size_t)REAL(r_val)[0];
                    return converted;
                } else {
                    Rf_error("Cannot convert to size_t");
                }
            } else if (type == &ffi_type_ssize_t_custom) {
                if (TYPEOF(r_val) == INTSXP) {
                    ssize_t* converted = (ssize_t*)R_alloc(1, sizeof(ssize_t));
                    *converted = (ssize_t)INTEGER(r_val)[0];
                    return converted;
                } else if (TYPEOF(r_val) == REALSXP) {
                    ssize_t* converted = (ssize_t*)R_alloc(1, sizeof(ssize_t));
                    *converted = (ssize_t)REAL(r_val)[0];
                    return converted;
                } else {
                    Rf_error("Cannot convert to ssize_t");
                }
            } else if (type == &ffi_type_bool_custom) {
                if (TYPEOF(r_val) == LGLSXP) {
                    int* converted = (int*)R_alloc(1, sizeof(int));
                    *converted = LOGICAL(r_val)[0];
                    return converted;
                } else if (TYPEOF(r_val) == INTSXP) {
                    int* converted = (int*)R_alloc(1, sizeof(int));
                    *converted = INTEGER(r_val)[0] != 0;
                    return converted;
                } else if (TYPEOF(r_val) == REALSXP) {
                    int* converted = (int*)R_alloc(1, sizeof(int));
                    *converted = REAL(r_val)[0] != 0.0;
                    return converted;
                } else {
                    Rf_error("Cannot convert to bool");
                }
            } else if (type == &ffi_type_wchar_t_custom) {
                if (TYPEOF(r_val) == INTSXP) {
                    wchar_t* converted = (wchar_t*)R_alloc(1, sizeof(wchar_t));
                    *converted = (wchar_t)INTEGER(r_val)[0];
                    return converted;
                } else if (TYPEOF(r_val) == STRSXP && LENGTH(r_val) > 0) {
                    wchar_t* converted = (wchar_t*)R_alloc(1, sizeof(wchar_t));
                    const char* str = CHAR(STRING_ELT(r_val, 0));
                    if (strlen(str) > 0) {
                        *converted = (wchar_t)str[0];
                    } else {
                        *converted = 0;
                    }
                    return converted;
                } else {
                    Rf_error("Cannot convert to wchar_t");
                }
            }
            Rf_error("Unsupported FFI type for conversion: %d", type->type);
            
        case FFI_TYPE_DOUBLE:
            if (TYPEOF(r_val) == REALSXP) {
                // Direct pointer to R's double data - safe with PROTECT
                return REAL(r_val);
            } else if (TYPEOF(r_val) == INTSXP) {
                double* converted = (double*)R_alloc(1, sizeof(double));
                *converted = (double)INTEGER(r_val)[0];
                return converted;
            } else {
                Rf_error("Cannot convert to double");
            }
            
        case FFI_TYPE_FLOAT: {
            if (TYPEOF(r_val) == REALSXP) {
                float* converted = (float*)R_alloc(1, sizeof(float));
                *converted = (float)REAL(r_val)[0];
                return converted;
            } else if (TYPEOF(r_val) == INTSXP) {
                float* converted = (float*)R_alloc(1, sizeof(float));
                *converted = (float)INTEGER(r_val)[0];
                return converted;
            } else {
                Rf_error("Cannot convert to float");
            }
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
SEXP convert_native_to_r(void* value, ffi_type* type) {
    switch (type->type) {
        case FFI_TYPE_VOID:
            return R_NilValue;
            
        case FFI_TYPE_SINT8:
            return ScalarInteger((int)(*(int8_t*)value));
            
        case FFI_TYPE_SINT16:
            return ScalarInteger((int)(*(int16_t*)value));
            
        case FFI_TYPE_SINT32:
            return ScalarInteger(*(int*)value);
            
        case FFI_TYPE_SINT64:
            return ScalarReal((double)(*(int64_t*)value));
            
        case FFI_TYPE_UINT8:
            return ScalarInteger((int)(*(uint8_t*)value));
            
        case FFI_TYPE_UINT16:
            return ScalarInteger((int)(*(uint16_t*)value));
            
        case FFI_TYPE_UINT32:
            return ScalarReal((double)(*(uint32_t*)value));
            
        case FFI_TYPE_UINT64:
            return ScalarReal((double)(*(uint64_t*)value));
            

            
        case FFI_TYPE_DOUBLE:
            return ScalarReal(*(double*)value);
            
        case FFI_TYPE_FLOAT:
            return ScalarReal((double)(*(float*)value));
            
        #if FFI_TYPE_LONGDOUBLE != FFI_TYPE_DOUBLE
        case FFI_TYPE_LONGDOUBLE:
            return ScalarReal((double)(*(long double*)value));
        #endif
            
        // Handle custom platform-dependent types by their underlying type
            
        case FFI_TYPE_POINTER: {
            void* ptr = *(void**)value;
            if (ptr == NULL) {
                return R_NilValue;
            }
            // Always return ExternalPtr for type safety - no dangerous heuristics
            // Users should use explicit string conversion functions when needed
            return R_MakeExternalPtr(ptr, Rf_install("generic_pointer"), R_NilValue);
        }
        
        default:
            // Check if this is one of our custom types and handle accordingly
            if (type == &ffi_type_string_custom) {
                char* str = *(char**)value;
                if (str == NULL) {
                    return R_NilValue;
                } else {
                    return mkString(str);
                }
            } else if (type == &ffi_type_size_t_custom) {
                return ScalarReal((double)(*(size_t*)value));
            } else if (type == &ffi_type_ssize_t_custom) {
                return ScalarReal((double)(*(ssize_t*)value));
            } else if (type == &ffi_type_bool_custom) {
                return ScalarLogical(*(int*)value);
            } else if (type == &ffi_type_wchar_t_custom) {
                wchar_t wc = *(wchar_t*)value;
                char str[2] = {0, 0};
                if (wc < 128) {  // ASCII range
                    str[0] = (char)wc;
                    return mkString(str);
                } else {
                    return ScalarInteger((int)wc);
                }
            }
            Rf_error("Unsupported FFI type for return conversion: %d", type->type);
    }
}

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

// Internal function that does the actual work
static SEXP do_ffi_call_internal(void* data) {
    SEXP* args = (SEXP*)data;
    SEXP r_cif = args[0];
    SEXP r_func_ptr = args[1];
    SEXP r_args = args[2];
    
    ffi_cif* cif = (ffi_cif*)R_ExternalPtrAddr(r_cif);
    void* func_ptr = R_ExternalPtrAddr(r_func_ptr);
    
    if (!cif) Rf_error("Invalid CIF pointer");
    if (!func_ptr) Rf_error("Invalid function pointer");
    
    int num_args = LENGTH(r_args);
    if (num_args != (int)cif->nargs) {
        Rf_error("Argument count mismatch: expected %d, got %d", (int)cif->nargs, num_args);
    }
    
    // Set up cleanup context
    ffi_call_context_t ctx = {0};
    
    // PROTECT r_args first
    PROTECT(r_args);
    ctx.protected_count = 1;
    
    void** arg_values = NULL;
    if (num_args > 0) {
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
SEXP R_ffi_call(SEXP r_cif, SEXP r_func_ptr, SEXP r_args) {
    // Use UNWIND_PROTECT for automatic cleanup on error
    SEXP args[3] = {r_cif, r_func_ptr, r_args};
    ffi_call_context_t ctx = {0};
    
    return R_UnwindProtect(do_ffi_call_internal, args, ffi_call_cleanup, &ctx, NULL);
}

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

// Simple offset calculation for struct fields
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

// Check if pointer is NULL
SEXP R_is_null_pointer(SEXP r_ptr) {
    if (TYPEOF(r_ptr) != EXTPTRSXP) {
        return ScalarLogical(TRUE);
    }
    
    void* ptr = R_ExternalPtrAddr(r_ptr);
    return ScalarLogical(ptr == NULL);
}



// Copy array from native memory (basic implementation)
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


SEXP R_libffi_version() {
#ifdef FFI_VERSION_STRING
    return mkString(FFI_VERSION_STRING);
#else
    return mkString("unknown");
#endif
}