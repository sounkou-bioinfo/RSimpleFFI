#include <R.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>
#include <stdint.h>

#include <stdbool.h>

/* Declare external functions */
SEXP R_get_builtin_ffi_type(SEXP name);
SEXP R_get_ffi_type_size(SEXP type_ptr);
SEXP R_create_struct_ffi_type(SEXP field_refs);
SEXP R_create_array_ffi_type(SEXP element_type, SEXP length);
SEXP R_prep_ffi_cif(SEXP return_type, SEXP arg_types);
SEXP R_ffi_call(SEXP cif_ptr, SEXP func_ptr, SEXP args, SEXP na_check);
SEXP R_alloc_struct(SEXP struct_type);
SEXP R_get_struct_field(SEXP ptr, SEXP field_index, SEXP struct_type);
SEXP R_set_struct_field(SEXP ptr, SEXP field_index, SEXP value, SEXP struct_type);
SEXP R_is_null_pointer(SEXP ptr);
SEXP R_copy_array(SEXP ptr, SEXP length, SEXP element_type);
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

/* pointer utility functions */
SEXP R_pointer_to_string(SEXP r_ptr);
SEXP R_make_typed_pointer(SEXP r_ptr, SEXP r_type_name);
SEXP R_get_pointer_type(SEXP r_ptr);

/* closure API functions */
SEXP R_ffi_closures_supported(void);
SEXP R_create_closure(SEXP r_function, SEXP r_cif);
SEXP R_get_closure_pointer(SEXP r_closure);

/* Registration table */
static const R_CallMethodDef CallEntries[] = {
    {"R_get_builtin_ffi_type",    (DL_FUNC) &R_get_builtin_ffi_type,    1},
    {"R_get_ffi_type_size",       (DL_FUNC) &R_get_ffi_type_size,       1},
    {"R_create_struct_ffi_type",  (DL_FUNC) &R_create_struct_ffi_type,  1},
    {"R_create_array_ffi_type",   (DL_FUNC) &R_create_array_ffi_type,   2},
    {"R_prep_ffi_cif",            (DL_FUNC) &R_prep_ffi_cif,            2},
    {"R_ffi_call",                (DL_FUNC) &R_ffi_call,                4},
    {"R_alloc_struct",            (DL_FUNC) &R_alloc_struct,            1},
    {"R_get_struct_field",        (DL_FUNC) &R_get_struct_field,        3},
    {"R_set_struct_field",        (DL_FUNC) &R_set_struct_field,        4},
    {"R_is_null_pointer",         (DL_FUNC) &R_is_null_pointer,         1},
    {"R_copy_array",              (DL_FUNC) &R_copy_array,              3},
    {"R_pointer_to_string",       (DL_FUNC) &R_pointer_to_string,       1},
    {"R_make_typed_pointer",      (DL_FUNC) &R_make_typed_pointer,      2},
    {"R_get_pointer_type",        (DL_FUNC) &R_get_pointer_type,        1},
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
    {NULL, NULL, 0}
};

void R_init_SimpleFFI(DllInfo *dll)
{
    R_registerRoutines(dll, CEntries, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}