/*
 * packed_structs.c - Test file for packed struct support
 * 
 * This file demonstrates passing packed structs to/from C functions.
 * Compile with: R CMD SHLIB packed_structs.c
 * 
 * Note: This file is in inst/extdata for testing purposes only,
 * not part of the main package build.
 */

#include <stdint.h>
#include <string.h>

/* Natural alignment struct: int (4) + padding (4) + double (8) = 16 bytes */
typedef struct {
    int32_t a;
    double b;
} NaturalStruct;

/* Packed struct: int (4) + double (8) = 12 bytes, no padding */
#pragma pack(push, 1)
typedef struct {
    int32_t a;
    double b;
} PackedStruct1;
#pragma pack(pop)

/* Pack=2 struct: int (4) + double (8) = 12 bytes */
#pragma pack(push, 2)
typedef struct {
    int32_t a;
    double b;
} PackedStruct2;
#pragma pack(pop)

/* More complex packed struct */
#pragma pack(push, 1)
typedef struct {
    uint8_t  flags;      /* offset 0, size 1 */
    uint32_t id;         /* offset 1, size 4 */
    uint16_t value;      /* offset 5, size 2 */
    double   data;       /* offset 7, size 8 */
} ComplexPacked;         /* total: 15 bytes */
#pragma pack(pop)

/* Return sizes for verification */
int get_natural_struct_size(void) {
    return sizeof(NaturalStruct);
}

int get_packed1_struct_size(void) {
    return sizeof(PackedStruct1);
}

int get_packed2_struct_size(void) {
    return sizeof(PackedStruct2);
}

int get_complex_packed_size(void) {
    return sizeof(ComplexPacked);
}

/* Functions that take packed structs by pointer */
int32_t get_packed1_a(PackedStruct1* s) {
    return s->a;
}

double get_packed1_b(PackedStruct1* s) {
    return s->b;
}

void set_packed1(PackedStruct1* s, int32_t a, double b) {
    s->a = a;
    s->b = b;
}

/* Sum fields of packed struct */
double sum_packed1(PackedStruct1* s) {
    return (double)s->a + s->b;
}

/* Complex packed struct accessors */
uint8_t get_complex_flags(ComplexPacked* s) {
    return s->flags;
}

uint32_t get_complex_id(ComplexPacked* s) {
    return s->id;
}

uint16_t get_complex_value(ComplexPacked* s) {
    return s->value;
}

double get_complex_data(ComplexPacked* s) {
    return s->data;
}

void set_complex(ComplexPacked* s, uint8_t flags, uint32_t id, 
                 uint16_t value, double data) {
    s->flags = flags;
    s->id = id;
    s->value = value;
    s->data = data;
}

/* Fill output struct with modified values */
void modify_packed1(PackedStruct1* in, PackedStruct1* out) {
    out->a = in->a * 2;
    out->b = in->b + 100.0;
}

/* Return offsets for verification */
int get_packed1_offset_a(void) {
    PackedStruct1 s;
    return (int)((char*)&s.a - (char*)&s);
}

int get_packed1_offset_b(void) {
    PackedStruct1 s;
    return (int)((char*)&s.b - (char*)&s);
}

int get_complex_offset_flags(void) {
    ComplexPacked s;
    return (int)((char*)&s.flags - (char*)&s);
}

int get_complex_offset_id(void) {
    ComplexPacked s;
    return (int)((char*)&s.id - (char*)&s);
}

int get_complex_offset_value(void) {
    ComplexPacked s;
    return (int)((char*)&s.value - (char*)&s);
}

int get_complex_offset_data(void) {
    ComplexPacked s;
    return (int)((char*)&s.data - (char*)&s);
}
