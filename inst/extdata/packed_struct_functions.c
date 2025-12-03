/*
 * packed_struct_functions.c
 *
 * Test functions for packed struct argument passing and return values.
 * This file uses #pragma pack to create packed structs that match
 * RSimpleFFI's ffi_struct(..., pack = 1) behavior.
 *
 * Compile with: gcc -shared -fPIC -o packed_struct_functions.so packed_struct_functions.c
 */

#include <stdio.h>
#include <stdint.h>
#include <string.h>

/* Packed struct: 5 bytes (1 + 4, no padding) */
#pragma pack(push, 1)
typedef struct {
    char a;
    int32_t b;
} PackedSimple;
#pragma pack(pop)

/* Normal struct for comparison: 8 bytes (1 + 3 padding + 4) */
typedef struct {
    char a;
    int32_t b;
} NormalSimple;

/* Nested packed struct */
#pragma pack(push, 1)
typedef struct {
    PackedSimple inner;
    char c;
} PackedNested;
#pragma pack(pop)

/* Packed struct with multiple fields */
#pragma pack(push, 1)
typedef struct {
    uint8_t  flags;
    uint32_t value;
    uint16_t count;
    uint8_t  status;
} PackedMulti;
#pragma pack(pop)

/* ============ Size verification functions ============ */

int get_packed_simple_size(void) {
    return sizeof(PackedSimple);
}

int get_normal_simple_size(void) {
    return sizeof(NormalSimple);
}

int get_packed_nested_size(void) {
    return sizeof(PackedNested);
}

int get_packed_multi_size(void) {
    return sizeof(PackedMulti);
}

/* ============ Offset verification functions ============ */

int get_packed_simple_b_offset(void) {
    return (int)((char*)&((PackedSimple*)0)->b - (char*)0);
}

int get_normal_simple_b_offset(void) {
    return (int)((char*)&((NormalSimple*)0)->b - (char*)0);
}

int get_packed_nested_c_offset(void) {
    return (int)((char*)&((PackedNested*)0)->c - (char*)0);
}

int get_packed_multi_value_offset(void) {
    return (int)((char*)&((PackedMulti*)0)->value - (char*)0);
}

int get_packed_multi_count_offset(void) {
    return (int)((char*)&((PackedMulti*)0)->count - (char*)0);
}

int get_packed_multi_status_offset(void) {
    return (int)((char*)&((PackedMulti*)0)->status - (char*)0);
}

/* ============ Functions that take packed struct BY POINTER ============ */

int32_t read_packed_simple_b(PackedSimple* p) {
    return p->b;
}

void write_packed_simple(PackedSimple* p, char a, int32_t b) {
    p->a = a;
    p->b = b;
}

int32_t sum_packed_multi(PackedMulti* p) {
    return (int32_t)p->flags + (int32_t)p->value + (int32_t)p->count + (int32_t)p->status;
}

void fill_packed_multi(PackedMulti* p, uint8_t flags, uint32_t value, uint16_t count, uint8_t status) {
    p->flags = flags;
    p->value = value;
    p->count = count;
    p->status = status;
}

/* ============ Functions that take packed struct BY VALUE ============ */
/* WARNING: These may not work correctly with libffi due to ABI issues */

int32_t read_packed_simple_b_byval(PackedSimple p) {
    return p.b;
}

PackedSimple make_packed_simple(char a, int32_t b) {
    PackedSimple result;
    result.a = a;
    result.b = b;
    return result;
}

/* ============ Array of packed structs ============ */

int32_t sum_packed_array(PackedSimple* arr, int count) {
    int32_t sum = 0;
    for (int i = 0; i < count; i++) {
        sum += arr[i].b;
    }
    return sum;
}

void fill_packed_array(PackedSimple* arr, int count, int32_t start_value) {
    for (int i = 0; i < count; i++) {
        arr[i].a = 'A' + i;
        arr[i].b = start_value + i * 10;
    }
}

/* ============ Verification helper ============ */

/* Returns 1 if the memory layout matches expected packed layout */
int verify_packed_simple_layout(void* ptr, char expected_a, int32_t expected_b) {
    char* bytes = (char*)ptr;
    
    /* In packed layout: a at offset 0, b at offset 1 */
    char actual_a = bytes[0];
    int32_t actual_b;
    memcpy(&actual_b, bytes + 1, sizeof(int32_t));
    
    return (actual_a == expected_a) && (actual_b == expected_b);
}
