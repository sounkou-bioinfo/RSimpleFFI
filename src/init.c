#include <R.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>
#include <stdint.h>

#include <stdbool.h>

/* Declare external functions */
SEXP R_get_builtin_ffi_type(SEXP name);
SEXP R_get_ffi_type_size(SEXP type_ptr);
SEXP R_get_ffi_type_alignment(SEXP type_ptr);
SEXP R_create_struct_ffi_type(SEXP field_refs, SEXP pack);
SEXP R_create_union_ffi_type(SEXP field_refs, SEXP pack);
SEXP R_create_array_ffi_type(SEXP element_type, SEXP length);
SEXP R_prep_ffi_cif(SEXP return_type, SEXP arg_types);
SEXP R_prep_ffi_cif_var(SEXP return_type, SEXP arg_types, SEXP nfixedargs);
SEXP R_ffi_call(SEXP cif_ptr, SEXP func_ptr, SEXP args, SEXP na_check);
SEXP R_alloc_struct(SEXP struct_type);
SEXP R_get_struct_field(SEXP ptr, SEXP field_index, SEXP struct_type);
SEXP R_set_struct_field(SEXP ptr, SEXP field_index, SEXP value, SEXP struct_type);
SEXP R_get_struct_field_at_offset(SEXP ptr, SEXP offset, SEXP field_type);
SEXP R_set_struct_field_at_offset(SEXP ptr, SEXP offset, SEXP value, SEXP field_type);
SEXP R_get_union_field(SEXP union_ptr, SEXP field_type);
SEXP R_set_union_field(SEXP union_ptr, SEXP value, SEXP field_type);
SEXP R_is_null_pointer(SEXP ptr);
SEXP R_copy_array(SEXP ptr, SEXP length, SEXP element_type);

/* Array of structs support */
SEXP R_alloc_struct_array(SEXP struct_type, SEXP n);
SEXP R_get_struct_array_element(SEXP ptr, SEXP index, SEXP struct_type);
SEXP R_get_struct_size(SEXP struct_type);
SEXP R_get_struct_num_fields(SEXP struct_type);
SEXP R_get_field_info(SEXP struct_type, SEXP field_index);
SEXP R_get_all_field_offsets(SEXP struct_type);

/* Packed struct support */
SEXP R_get_packed_field_offset(SEXP struct_type, SEXP field_index, SEXP pack);
SEXP R_get_all_packed_field_offsets(SEXP struct_type, SEXP pack);
SEXP R_get_packed_struct_size(SEXP struct_type, SEXP pack);
SEXP R_get_libffi_struct_offsets(SEXP struct_type);
SEXP R_get_struct_byval_type(SEXP struct_type);

SEXP R_alloc_buffer(SEXP r_size);
SEXP R_alloc_typed_buffer(SEXP r_type, SEXP r_n);
SEXP R_fill_typed_buffer(SEXP r_ptr, SEXP r_vals, SEXP r_type);
SEXP R_ffi_free(SEXP r_ptr);
/*libffi versions*/
SEXP R_libffi_version(void);
/* Declare test functions */
double test_add_double(double a, double b);
double test_square(double x);
int test_add_int(int a, int b);
float test_add_float(float a, float b);
void test_void_function();
bool test_bool_func(bool value);
void* test_return_pointer(void* ptr);

// Additional test functions for extended types
// Extended type test functions
int test_factorial(int n);
int8_t test_int8_func(int8_t a);
int16_t test_int16_func(int16_t a);  
int64_t test_int64_func(int64_t a);
uint8_t test_uint8_func(uint8_t a);
uint16_t test_uint16_func(uint16_t a);
uint32_t test_uint32_func(uint32_t a);
uint64_t test_uint64_func(uint64_t a);
long test_long_func(long a);
unsigned long test_ulong_func(unsigned long a);

// Structure test functions
typedef struct {
    int x;
    int y;
} Point2D;

void test_move_point2d(Point2D* point, int dx, int dy);
Point2D test_create_point2d(int x, int y);
int test_get_point_x(Point2D* point);
int test_get_point_y(Point2D* point);
/* Callback test function declarations */
int test_callback(int (*func)(int), int value);
double test_double_callback(double (*func)(double), double value);
double test_transform_sum(double* arr, int len, double (*transform)(double));
int test_find_max(int* arr, int len, int (*cmp)(int, int));
void test_foreach(int start, int end, void (*callback)(int));

/* Struct array test functions */
typedef struct { int a; double b; } MixedStruct;
typedef struct { char c; int i; double d; } AlignedStruct;

int test_sum_point2d_array(Point2D* points, int count);
void test_scale_point2d_array(Point2D* points, int count);
double test_sum_mixed_array(MixedStruct* arr, int count);
void test_init_mixed_array(MixedStruct* arr, int count);
Point2D test_get_point2d_at(Point2D* points, int index);
void test_set_point2d_at(Point2D* points, int index, int x, int y);
double test_sum_aligned_array(AlignedStruct* arr, int count);
void test_init_aligned_array(AlignedStruct* arr, int count);
int test_sizeof_mixed(void);
int test_sizeof_aligned(void);
int test_sizeof_point2d(void);

/* Varargs test functions */
double test_varargs_sum(int nargs, ...);
double test_varargs_sum_doubles(int nargs, ...);
int test_varargs_mixed(const char* prefix, int nargs, ...);
double test_varargs_mixed_types(int npairs, ...);

/* 64-bit bitfield test functions */
uint64_t test_bitfield64_pack(uint32_t low, uint32_t high);
uint32_t test_bitfield64_get_low(uint64_t packed);
uint32_t test_bitfield64_get_high(uint64_t packed);

/* Signed bitfield test functions */
uint16_t test_signed_bitfield_pack(int val4, int val5);
int test_signed_bitfield_get4(uint16_t packed);
int test_signed_bitfield_get5(uint16_t packed);

/* Long long test functions */
long long test_longlong_func(long long a);
unsigned long long test_ulonglong_func(unsigned long long a);

/* pointer utility functions */
SEXP R_pointer_to_string(SEXP r_ptr);
SEXP R_make_typed_pointer(SEXP r_ptr, SEXP r_type_name);
SEXP R_get_pointer_type(SEXP r_ptr);
SEXP R_deref_pointer(SEXP r_ptr);
SEXP R_read_global(SEXP r_ptr, SEXP r_type);

/* closure API functions */
SEXP R_ffi_closures_supported(void);
SEXP R_create_closure(SEXP r_function, SEXP r_cif);
SEXP R_get_closure_pointer(SEXP r_closure);

/* 64-bit bitfield operations */
SEXP R_ffi_pack_bits64(SEXP r_values, SEXP r_widths);
SEXP R_ffi_unpack_bits64(SEXP r_packed, SEXP r_widths);
SEXP R_ffi_extract_bits64(SEXP r_packed, SEXP r_offset, SEXP r_width);
SEXP R_ffi_extract_signed_bits64(SEXP r_packed, SEXP r_offset, SEXP r_width);
SEXP R_ffi_set_bits64(SEXP r_packed, SEXP r_value, SEXP r_offset, SEXP r_width);

/* Registration table */
static const R_CallMethodDef CallEntries[] = {
    {"R_get_builtin_ffi_type",    (DL_FUNC) &R_get_builtin_ffi_type,    1},
    {"R_get_ffi_type_size",       (DL_FUNC) &R_get_ffi_type_size,       1},
    {"R_get_ffi_type_alignment",  (DL_FUNC) &R_get_ffi_type_alignment,  1},
    {"R_create_struct_ffi_type",  (DL_FUNC) &R_create_struct_ffi_type,  2},
    {"R_create_union_ffi_type",   (DL_FUNC) &R_create_union_ffi_type,   2},
    {"R_create_array_ffi_type",   (DL_FUNC) &R_create_array_ffi_type,   2},
    {"R_prep_ffi_cif",            (DL_FUNC) &R_prep_ffi_cif,            2},
    {"R_prep_ffi_cif_var",        (DL_FUNC) &R_prep_ffi_cif_var,        3},
    {"R_ffi_call",                (DL_FUNC) &R_ffi_call,                4},
    {"R_alloc_struct",            (DL_FUNC) &R_alloc_struct,            1},
    {"R_get_struct_field",        (DL_FUNC) &R_get_struct_field,        3},
    {"R_set_struct_field",        (DL_FUNC) &R_set_struct_field,        4},
    {"R_get_struct_field_at_offset", (DL_FUNC) &R_get_struct_field_at_offset, 3},
    {"R_set_struct_field_at_offset", (DL_FUNC) &R_set_struct_field_at_offset, 4},
    {"R_get_union_field",         (DL_FUNC) &R_get_union_field,         2},
    {"R_set_union_field",         (DL_FUNC) &R_set_union_field,         3},
    {"R_is_null_pointer",         (DL_FUNC) &R_is_null_pointer,         1},
    {"R_copy_array",              (DL_FUNC) &R_copy_array,              3},
    /* Array of structs support */
    {"R_alloc_struct_array",      (DL_FUNC) &R_alloc_struct_array,      2},
    {"R_get_struct_array_element",(DL_FUNC) &R_get_struct_array_element,3},
    {"R_get_struct_size",         (DL_FUNC) &R_get_struct_size,         1},
    {"R_get_struct_num_fields",   (DL_FUNC) &R_get_struct_num_fields,   1},
    {"R_get_field_info",          (DL_FUNC) &R_get_field_info,          2},
    {"R_get_all_field_offsets",   (DL_FUNC) &R_get_all_field_offsets,   1},
    /* Packed struct support */
    {"R_get_packed_field_offset",      (DL_FUNC) &R_get_packed_field_offset,      3},
    {"R_get_all_packed_field_offsets", (DL_FUNC) &R_get_all_packed_field_offsets, 2},
    {"R_get_packed_struct_size",       (DL_FUNC) &R_get_packed_struct_size,       2},
    {"R_get_libffi_struct_offsets",    (DL_FUNC) &R_get_libffi_struct_offsets,    1},
    {"R_get_struct_byval_type",        (DL_FUNC) &R_get_struct_byval_type,        1},
    {"R_pointer_to_string",       (DL_FUNC) &R_pointer_to_string,       1},
    {"R_make_typed_pointer",      (DL_FUNC) &R_make_typed_pointer,      2},
    {"R_get_pointer_type",        (DL_FUNC) &R_get_pointer_type,        1},
    {"R_deref_pointer",           (DL_FUNC) &R_deref_pointer,           1},
    {"R_read_global",             (DL_FUNC) &R_read_global,             2},
    {"R_get_builtin_ffi_type",    (DL_FUNC) &R_get_builtin_ffi_type,    1},
    {"R_get_ffi_type_size",       (DL_FUNC) &R_get_ffi_type_size,       1},
    {"R_alloc_buffer",            (DL_FUNC) &R_alloc_buffer,            1},
    {"R_alloc_typed_buffer",      (DL_FUNC) &R_alloc_typed_buffer,      2},
    {"R_fill_typed_buffer",       (DL_FUNC) &R_fill_typed_buffer,       3},
    {"R_ffi_free",                (DL_FUNC) &R_ffi_free,                1},
    {"R_libffi_version",          (DL_FUNC) &R_libffi_version,          0},
    /* closure API */
    {"R_ffi_closures_supported",  (DL_FUNC) &R_ffi_closures_supported,  0},
    {"R_create_closure",          (DL_FUNC) &R_create_closure,          2},
    {"R_get_closure_pointer",     (DL_FUNC) &R_get_closure_pointer,     1},
    /* 64-bit bitfield operations */
    {"R_ffi_pack_bits64",         (DL_FUNC) &R_ffi_pack_bits64,         2},
    {"R_ffi_unpack_bits64",       (DL_FUNC) &R_ffi_unpack_bits64,       2},
    {"R_ffi_extract_bits64",      (DL_FUNC) &R_ffi_extract_bits64,      3},
    {"R_ffi_extract_signed_bits64",(DL_FUNC) &R_ffi_extract_signed_bits64, 3},
    {"R_ffi_set_bits64",          (DL_FUNC) &R_ffi_set_bits64,          4},
    {NULL, NULL, 0}
};


/* C method table for test functions */
static const R_CMethodDef CEntries[] = {
    {"test_add_double", (DL_FUNC) &test_add_double, 2},
    {"test_square", (DL_FUNC) &test_square, 1},
    {"test_add_int", (DL_FUNC) &test_add_int, 2},
    {"test_add_float", (DL_FUNC) &test_add_float, 2},
    {"test_void_function", (DL_FUNC) &test_void_function, 0},
    {"test_factorial", (DL_FUNC) &test_factorial, 1},
    {"test_bool_func", (DL_FUNC) &test_bool_func, 1},
    {"test_return_pointer", (DL_FUNC) &test_return_pointer, 1},
    
    // Extended type test functions
    {"test_int8_func", (DL_FUNC) &test_int8_func, 1},
    {"test_int16_func", (DL_FUNC) &test_int16_func, 1},
    {"test_int64_func", (DL_FUNC) &test_int64_func, 1},
    {"test_uint8_func", (DL_FUNC) &test_uint8_func, 1},
    {"test_uint16_func", (DL_FUNC) &test_uint16_func, 1},
    {"test_uint32_func", (DL_FUNC) &test_uint32_func, 1},
    {"test_uint64_func", (DL_FUNC) &test_uint64_func, 1},
    {"test_long_func", (DL_FUNC) &test_long_func, 1},
    {"test_ulong_func", (DL_FUNC) &test_ulong_func, 1},
    
    // Structure test functions
    {"test_move_point2d", (DL_FUNC) &test_move_point2d, 3},
    {"test_create_point2d", (DL_FUNC) &test_create_point2d, 2},
    {"test_get_point_x", (DL_FUNC) &test_get_point_x, 1},
    {"test_get_point_y", (DL_FUNC) &test_get_point_y, 1},
    
    // Callback test functions
    {"test_callback", (DL_FUNC) &test_callback, 2},
    {"test_double_callback", (DL_FUNC) &test_double_callback, 2},
    {"test_transform_sum", (DL_FUNC) &test_transform_sum, 3},
    {"test_find_max", (DL_FUNC) &test_find_max, 3},
    {"test_foreach", (DL_FUNC) &test_foreach, 3},
    
    // Struct array test functions
    {"test_sum_point2d_array", (DL_FUNC) &test_sum_point2d_array, 2},
    {"test_scale_point2d_array", (DL_FUNC) &test_scale_point2d_array, 2},
    {"test_sum_mixed_array", (DL_FUNC) &test_sum_mixed_array, 2},
    {"test_init_mixed_array", (DL_FUNC) &test_init_mixed_array, 2},
    {"test_get_point2d_at", (DL_FUNC) &test_get_point2d_at, 2},
    {"test_set_point2d_at", (DL_FUNC) &test_set_point2d_at, 4},
    {"test_sum_aligned_array", (DL_FUNC) &test_sum_aligned_array, 2},
    {"test_init_aligned_array", (DL_FUNC) &test_init_aligned_array, 2},
    {"test_sizeof_mixed", (DL_FUNC) &test_sizeof_mixed, 0},
    {"test_sizeof_aligned", (DL_FUNC) &test_sizeof_aligned, 0},
    {"test_sizeof_point2d", (DL_FUNC) &test_sizeof_point2d, 0},
    
    // Varargs test functions (arg count is for fixed args only)
    {"test_varargs_sum", (DL_FUNC) &test_varargs_sum, 1},
    {"test_varargs_sum_doubles", (DL_FUNC) &test_varargs_sum_doubles, 1},
    {"test_varargs_mixed", (DL_FUNC) &test_varargs_mixed, 2},
    {"test_varargs_mixed_types", (DL_FUNC) &test_varargs_mixed_types, 1},
    
    // 64-bit bitfield test functions
    {"test_bitfield64_pack", (DL_FUNC) &test_bitfield64_pack, 2},
    {"test_bitfield64_get_low", (DL_FUNC) &test_bitfield64_get_low, 1},
    {"test_bitfield64_get_high", (DL_FUNC) &test_bitfield64_get_high, 1},
    
    // Signed bitfield test functions
    {"test_signed_bitfield_pack", (DL_FUNC) &test_signed_bitfield_pack, 2},
    {"test_signed_bitfield_get4", (DL_FUNC) &test_signed_bitfield_get4, 1},
    {"test_signed_bitfield_get5", (DL_FUNC) &test_signed_bitfield_get5, 1},
    
    // Long long test functions
    {"test_longlong_func", (DL_FUNC) &test_longlong_func, 1},
    {"test_ulonglong_func", (DL_FUNC) &test_ulonglong_func, 1},
    {NULL, NULL, 0}
};

void R_init_SimpleFFI(DllInfo *dll)
{
    R_registerRoutines(dll, CEntries, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}