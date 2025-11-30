/* Test C functions for RSimpleFFI */
#include <stdbool.h>
#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <math.h>

/* Test functions for basic FFI operations */

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

// Callback testing (basic)
typedef int (*IntCallback)(int);

int test_callback(IntCallback func, int value) {
    return func(value);
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
