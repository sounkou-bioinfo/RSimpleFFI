#ifndef COMPLEX_TYPES_H
#define COMPLEX_TYPES_H

#include <stdint.h>

/* Test header with more complex type patterns */

#define MAX_ARRAY_SIZE 256
#define DEFAULT_VALUE 0

// Struct with arrays
struct ArrayStruct {
    int values[10];
    char name[64];
    double matrix[3][3];
};

// Struct with pointers
struct PointerStruct {
    int *data;
    char **strings;
    void *opaque;
};

// Nested struct
struct Outer {
    int id;
    struct Inner {
        float x;
        float y;
    } position;
};

// Function pointers in parameters
typedef void (*Callback)(int status, void *user_data);

// Functions with complex signatures
int process_array(int arr[], size_t len);
char* get_string(const char *key);
void register_callback(Callback cb, void *data);
uint64_t compute_hash(const void *data, size_t len);

#endif
