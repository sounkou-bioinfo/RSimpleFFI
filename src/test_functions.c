/* Test C functions for RSimpleFFI */
#include <stdbool.h>
#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <math.h>

/* ==========================================================================
 * Test Global Variables
 * These are used to test ffi_deref_pointer and ffi_read_global
 * ==========================================================================*/

/* Global integer */
int test_global_int = 42;

/* Global double */
double test_global_double = 3.14159;

/* Global pointer (to a string) */
const char* test_global_string = "Hello from global!";

/* Global struct for testing */
typedef struct {
    int x;
    int y;
} TestPoint;

TestPoint test_global_point = { 100, 200 };

/* Global pointer to a struct */
TestPoint* test_global_point_ptr = &test_global_point;

/* Global array */
int test_global_array[5] = { 10, 20, 30, 40, 50 };


/* ==========================================================================
 * Test functions for basic FFI operations
 * ==========================================================================*/

/* Simple math functions */

/* Double operations */
double test_add_double(double a, double b) {
    return a + b;
}

double test_multiply_double(double a, double b) {
    return a * b;
}

double test_square(double x) {
    return x * x;
}

/* Integer operations */
int test_add_int(int a, int b) {
    return a + b;
}

int test_factorial(int n) {
    if (n <= 1) return 1;
    return n * test_factorial(n - 1);
}

/* Float operations */
float test_add_float(float a, float b) {
    return a + b;
}

/* Void function */
void test_void_function() {
    /* Does nothing, just for testing void calls */
}

/* Boolean operations */
bool test_bool_func(bool value) {
    return !value;  // Return the opposite for testing
}

/* Pointer operations */
void* test_return_pointer(void* ptr) {
    return ptr;
}

double* test_array_sum(double* arr, int len, double* result) {
    *result = 0.0;
    for (int i = 0; i < len; i++) {
        *result += arr[i];
    }
    return result;
}

// Modify array in place
void test_double_array(double* arr, int len) {
    for (int i = 0; i < len; i++) {
        arr[i] *= 2.0;
    }
}

// Structure tests
typedef struct {
    int x;
    int y;
} Point2D;

typedef struct {
    double x;
    double y;
    double z;
} Point3D;

typedef struct {
    int id;
    double value;
    char name[32];
} Record;

// Structure operations
Point2D test_create_point2d(int x, int y) {
    Point2D p = {x, y};
    return p;
}

double test_point2d_distance(Point2D* p1, Point2D* p2) {
    int dx = p2->x - p1->x;
    int dy = p2->y - p1->y;
    return sqrt(dx*dx + dy*dy);
}

void test_move_point2d(Point2D* p, int dx, int dy) {
    p->x += dx;
    p->y += dy;
}

int test_get_point_x(Point2D* point) {
    return point ? point->x : 0;
}

int test_get_point_y(Point2D* point) {
    return point ? point->y : 0;
}

Point3D test_create_point3d(double x, double y, double z) {
    Point3D p = {x, y, z};
    return p;
}

double test_point3d_magnitude(Point3D* p) {
    return sqrt(p->x*p->x + p->y*p->y + p->z*p->z);
}

// String operations
int test_string_length(const char* str) {
    return strlen(str);
}

const char* test_return_string() {
    return "Hello from C!";
}

// Memory allocation tests
double* test_alloc_double_array(int size) {
    return (double*)calloc(size, sizeof(double));
}

void test_fill_array(double* arr, int size, double value) {
    for (int i = 0; i < size; i++) {
        arr[i] = value + i;
    }
}

void test_free_array(double* arr) {
    free(arr);
}

// Error testing
int test_divide_int(int a, int b) {
    if (b == 0) return -1; // Error indicator
    return a / b;
}

// Callback testing
typedef int (*IntCallback)(int);
typedef int (*IntCmpCallback)(int, int);
typedef double (*DoubleCallback)(double);
typedef void (*VoidIntCallback)(int);

// Simple callback that applies function to value
int test_callback(IntCallback func, int value) {
    return func(value);
}

// Apply a callback to transform a value
double test_double_callback(DoubleCallback func, double value) {
    return func(value);
}

// Sum array using callback for transformation
double test_transform_sum(double* arr, int len, DoubleCallback transform) {
    double sum = 0.0;
    for (int i = 0; i < len; i++) {
        sum += transform(arr[i]);
    }
    return sum;
}

// Find max using comparison callback
int test_find_max(int* arr, int len, IntCmpCallback cmp) {
    if (len <= 0) return 0;
    int max_val = arr[0];
    for (int i = 1; i < len; i++) {
        if (cmp(arr[i], max_val) > 0) {
            max_val = arr[i];
        }
    }
    return max_val;
}

// Call callback with sequence of values
void test_foreach(int start, int end, VoidIntCallback callback) {
    for (int i = start; i < end; i++) {
        callback(i);
    }
}

// Multi-argument function
double test_many_args(int a, float b, double c, int d, float e) {
    return (double)a + (double)b + c + (double)d + (double)e;
}

// Performance test function
void test_performance_loop(int iterations) {
    volatile int sum = 0;
    for (int i = 0; i < iterations; i++) {
        sum += i;
    }
}

/* Extended type test functions */

// 8-bit integer functions
int8_t test_int8_func(int8_t a) {
    return a + 1;
}

uint8_t test_uint8_func(uint8_t a) {
    return a + 1;
}

// 16-bit integer functions  
int16_t test_int16_func(int16_t a) {
    return a * 2;
}

uint16_t test_uint16_func(uint16_t a) {
    return a * 2;
}

// 32-bit integer functions
uint32_t test_uint32_func(uint32_t a) {
    return a * 3;
}

// 64-bit integer functions
int64_t test_int64_func(int64_t a) {
    return a * 4;
}

uint64_t test_uint64_func(uint64_t a) {
    return a * 4;
}

// Long integer functions
long test_long_func(long a) {
    return a * 5;
}

unsigned long test_ulong_func(unsigned long a) {
    return a * 5;
}


/* ============================================================================
 * STRUCT ARRAY TEST FUNCTIONS
 * ============================================================================
 */

/* Struct with alignment padding: int (4) + padding (4) + double (8) = 16 */
typedef struct {
    int a;
    double b;
} MixedStruct;

/* Sum all x values in an array of Point2D */
int test_sum_point2d_array(Point2D* points, int count) {
    int sum = 0;
    for (int i = 0; i < count; i++) {
        sum += points[i].x + points[i].y;
    }
    return sum;
}

/* Modify each point in array: multiply x by 2, y by 3 */
void test_scale_point2d_array(Point2D* points, int count) {
    for (int i = 0; i < count; i++) {
        points[i].x *= 2;
        points[i].y *= 3;
    }
}

/* Sum all 'a' and 'b' fields in MixedStruct array */
double test_sum_mixed_array(MixedStruct* arr, int count) {
    double sum = 0.0;
    for (int i = 0; i < count; i++) {
        sum += (double)arr[i].a + arr[i].b;
    }
    return sum;
}

/* Initialize MixedStruct array with index-based values */
void test_init_mixed_array(MixedStruct* arr, int count) {
    for (int i = 0; i < count; i++) {
        arr[i].a = i + 1;
        arr[i].b = (i + 1) * 0.5;
    }
}

/* Get element at index from Point2D array (returns by value) */
Point2D test_get_point2d_at(Point2D* points, int index) {
    return points[index];
}

/* Set element at index in Point2D array */
void test_set_point2d_at(Point2D* points, int index, int x, int y) {
    points[index].x = x;
    points[index].y = y;
}

/* Struct with char for alignment testing: char (1) + pad (3) + int (4) + double (8) */
typedef struct {
    char c;
    int i;
    double d;
} AlignedStruct;

/* Sum aligned struct array */
double test_sum_aligned_array(AlignedStruct* arr, int count) {
    double sum = 0.0;
    for (int i = 0; i < count; i++) {
        sum += (double)arr[i].c + (double)arr[i].i + arr[i].d;
    }
    return sum;
}

/* Initialize aligned struct array */
void test_init_aligned_array(AlignedStruct* arr, int count) {
    for (int i = 0; i < count; i++) {
        arr[i].c = 'A' + (i % 26);
        arr[i].i = (i + 1) * 10;
        arr[i].d = (i + 1) * 1.5;
    }
}

/* Return size of MixedStruct (for verification) */
int test_sizeof_mixed(void) {
    return (int)sizeof(MixedStruct);
}

/* Return size of AlignedStruct (for verification) */
int test_sizeof_aligned(void) {
    return (int)sizeof(AlignedStruct);
}

/* Return size of Point2D (for verification) */
int test_sizeof_point2d(void) {
    return (int)sizeof(Point2D);
}


/* ============================================================================
 * VARARGS TEST FUNCTIONS
 * ============================================================================
 */

#include <stdarg.h>

/* Sum nargs integers passed as varargs */
double test_varargs_sum(int nargs, ...) {
    double sum = 0;
    va_list va;
    va_start(va, nargs);
    
    for (int i = 0; i < nargs; i++) {
        sum += va_arg(va, int);
    }
    
    va_end(va);
    return sum;
}

/* Sum doubles passed as varargs (NULL-terminated via count) */
double test_varargs_sum_doubles(int nargs, ...) {
    double sum = 0;
    va_list va;
    va_start(va, nargs);
    
    for (int i = 0; i < nargs; i++) {
        sum += va_arg(va, double);
    }
    
    va_end(va);
    return sum;
}

/* Mixed fixed + varargs: format string + nargs integers */
int test_varargs_mixed(const char* prefix, int nargs, ...) {
    int sum = 0;
    va_list va;
    va_start(va, nargs);
    
    /* Just sum the integers, ignore prefix */
    (void)prefix;
    for (int i = 0; i < nargs; i++) {
        sum += va_arg(va, int);
    }
    
    va_end(va);
    return sum;
}

/* Truly mixed varargs: alternating int and double
 * format: int count, then pairs of (int, double) */
double test_varargs_mixed_types(int npairs, ...) {
    double sum = 0;
    va_list va;
    va_start(va, npairs);
    
    for (int i = 0; i < npairs; i++) {
        int ival = va_arg(va, int);
        double dval = va_arg(va, double);
        sum += (double)ival + dval;
    }
    
    va_end(va);
    return sum;
}

/* ==========================================================================
 * Bit-field Test Functions
 * 
 * These test that our manual bit-packing strategy works correctly when
 * interfacing with C functions that expect bit-field structs.
 * ==========================================================================*/

/* Test bit-field struct - simulated as uint32_t for FFI */
typedef struct {
    unsigned int enabled : 1;
    unsigned int mode : 3;
    unsigned int priority : 4;
    unsigned int reserved : 24;
} SettingsFlags;

/* Union to access bit-field struct as uint32_t */
typedef union {
    SettingsFlags flags;
    uint32_t packed;
} SettingsUnion;

/* Extract enabled flag from packed uint32_t */
int test_bitfield_get_enabled(uint32_t packed) {
    SettingsUnion u;
    u.packed = packed;
    return u.flags.enabled;
}

/* Extract mode from packed uint32_t */
int test_bitfield_get_mode(uint32_t packed) {
    SettingsUnion u;
    u.packed = packed;
    return u.flags.mode;
}

/* Extract priority from packed uint32_t */
int test_bitfield_get_priority(uint32_t packed) {
    SettingsUnion u;
    u.packed = packed;
    return u.flags.priority;
}

/* Create packed uint32_t from individual fields */
uint32_t test_bitfield_pack(int enabled, int mode, int priority) {
    SettingsUnion u;
    u.packed = 0;
    u.flags.enabled = enabled & 0x1;
    u.flags.mode = mode & 0x7;
    u.flags.priority = priority & 0xF;
    u.flags.reserved = 0;
    return u.packed;
}

/* Verify a packed value has expected field values */
int test_bitfield_verify(uint32_t packed, int expected_enabled, 
                         int expected_mode, int expected_priority) {
    SettingsUnion u;
    u.packed = packed;
    
    return (u.flags.enabled == (unsigned int)(expected_enabled & 0x1)) &&
           (u.flags.mode == (unsigned int)(expected_mode & 0x7)) &&
           (u.flags.priority == (unsigned int)(expected_priority & 0xF));
}

/* Increment priority field, wrapping at 15 */
uint32_t test_bitfield_increment_priority(uint32_t packed) {
    SettingsUnion u;
    u.packed = packed;
    u.flags.priority = (u.flags.priority + 1) & 0xF;
    return u.packed;
}

/* Toggle enabled flag */
uint32_t test_bitfield_toggle_enabled(uint32_t packed) {
    SettingsUnion u;
    u.packed = packed;
    u.flags.enabled = !u.flags.enabled;
    return u.packed;
}

/* Test with a smaller bit-field struct (8-bit) */
typedef struct {
    unsigned int syn : 1;
    unsigned int ack : 1;
    unsigned int fin : 1;
    unsigned int rst : 1;
    unsigned int reserved : 4;
} PacketFlags;

typedef union {
    PacketFlags flags;
    uint8_t packed;
} PacketUnion;

/* Check if ACK flag is set */
int test_packet_has_ack(uint8_t packed) {
    PacketUnion u;
    u.packed = packed;
    return u.flags.ack;
}

/* Create SYN+ACK packet */
uint8_t test_packet_create_synack(void) {
    PacketUnion u;
    u.packed = 0;
    u.flags.syn = 1;
    u.flags.ack = 1;
    u.flags.fin = 0;
    u.flags.rst = 0;
    return u.packed;
}

/* Count set flags in packet */
int test_packet_count_flags(uint8_t packed) {
    PacketUnion u;
    u.packed = packed;
    return u.flags.syn + u.flags.ack + u.flags.fin + u.flags.rst;
}


/* ==========================================================================
 * Bit-field Struct Pass-by-Value Tests
 * 
 * These test passing bit-field structs directly (not as packed integers).
 * This tests whether libffi can handle bit-field structs at all.
 * ==========================================================================*/

/* Function that takes bit-field struct by value */
int test_bitfield_struct_get_mode(SettingsFlags settings) {
    return settings.mode;
}

/* Function that returns bit-field struct by value */
SettingsFlags test_bitfield_struct_create(int enabled, int mode, int priority) {
    SettingsFlags settings;
    settings.enabled = enabled & 0x1;
    settings.mode = mode & 0x7;
    settings.priority = priority & 0xF;
    settings.reserved = 0;
    return settings;
}

/* Function that takes pointer to bit-field struct */
int test_bitfield_struct_ptr_get_mode(SettingsFlags* settings) {
    return settings->mode;
}

/* Function that modifies bit-field struct via pointer */
void test_bitfield_struct_ptr_set_mode(SettingsFlags* settings, int mode) {
    settings->mode = mode & 0x7;
}

/* Function combining both: takes struct by value, returns modified struct */
SettingsFlags test_bitfield_struct_increment_priority(SettingsFlags settings) {
    settings.priority = (settings.priority + 1) & 0xF;
    return settings;
}
