#include <R.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>
#include <stdint.h>

/* Declare external functions */
SEXP R_get_builtin_ffi_type(SEXP name);
SEXP R_get_ffi_type_size(SEXP type_ptr);
SEXP R_create_struct_ffi_type(SEXP field_refs);
SEXP R_prep_ffi_cif(SEXP return_type, SEXP arg_types);
SEXP R_ffi_call(SEXP cif_ptr, SEXP func_ptr, SEXP args);
SEXP R_alloc_struct(SEXP struct_type);
SEXP R_get_struct_field(SEXP ptr, SEXP field_index, SEXP struct_type);
SEXP R_set_struct_field(SEXP ptr, SEXP field_index, SEXP value, SEXP struct_type);
SEXP R_is_null_pointer(SEXP ptr);
SEXP R_copy_array(SEXP ptr, SEXP length, SEXP element_type);

// Dynamic library loading functions using R's dynload
SEXP R_dyn_load(SEXP filename, SEXP local, SEXP now);
SEXP R_dyn_unload(SEXP dll_info);
SEXP R_dyn_symbol(SEXP dll_info, SEXP symbol_name);
SEXP R_find_symbol(SEXP symbol_name, SEXP package_name);
SEXP R_get_dll_info(SEXP dll_info);

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

/* pointer utility functions */
SEXP R_pointer_to_string(SEXP r_ptr);
SEXP R_make_typed_pointer(SEXP r_ptr, SEXP r_type_name);
SEXP R_get_pointer_type(SEXP r_ptr);

/* Registration table */
static const R_CallMethodDef CallEntries[] = {
    {"R_get_builtin_ffi_type",    (DL_FUNC) &R_get_builtin_ffi_type,    1},
    {"R_get_ffi_type_size",       (DL_FUNC) &R_get_ffi_type_size,       1},
    {"R_create_struct_ffi_type",  (DL_FUNC) &R_create_struct_ffi_type,  1},
    {"R_prep_ffi_cif",            (DL_FUNC) &R_prep_ffi_cif,            2},
    {"R_ffi_call",                (DL_FUNC) &R_ffi_call,                3},
    {"R_alloc_struct",            (DL_FUNC) &R_alloc_struct,            1},
    {"R_get_struct_field",        (DL_FUNC) &R_get_struct_field,        3},
    {"R_set_struct_field",        (DL_FUNC) &R_set_struct_field,        4},
    {"R_is_null_pointer",         (DL_FUNC) &R_is_null_pointer,         1},
    {"R_copy_array",              (DL_FUNC) &R_copy_array,              3},
    {"R_pointer_to_string",       (DL_FUNC) &R_pointer_to_string,       1},
    {"R_make_typed_pointer",      (DL_FUNC) &R_make_typed_pointer,      2},
    {"R_get_pointer_type",        (DL_FUNC) &R_get_pointer_type,        1},
    
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
    {NULL, NULL, 0}
};

void R_init_SimpleFFI(DllInfo *dll)
{
    R_registerRoutines(dll, CEntries, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}