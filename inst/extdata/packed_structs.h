/* packed_structs.h - Test header for packed struct support in RSimpleFFI
 * 
 * This header demonstrates how to define and use packed structs with RSimpleFFI.
 * Packed structs remove padding between fields, useful for binary protocols,
 * file formats, and hardware registers.
 */

#ifndef PACKED_STRUCTS_H
#define PACKED_STRUCTS_H

#include <stdint.h>

/* ==========================================================================
 * Size Comparison: Natural vs Packed
 * ==========================================================================
 * 
 * Natural alignment (default):
 *   struct { char a; int b; char c; double d; }
 *   Layout: [a][pad][pad][pad][b b b b][c][pad...][d d d d d d d d]
 *   Size: 24 bytes
 * 
 * Pack=1 (no padding):
 *   #pragma pack(push, 1) struct { ... } #pragma pack(pop)
 *   Layout: [a][b b b b][c][d d d d d d d d]
 *   Size: 14 bytes
 * 
 * Pack=2:
 *   Layout: [a][pad][b b b b][c][pad][d d d d d d d d]
 *   Size: 16 bytes
 * 
 * Pack=4:
 *   Layout: [a][pad][pad][pad][b b b b][c][pad][pad][pad][d d d d d d d d]
 *   Size: 20 bytes
 */

/* Natural alignment struct (has padding) */
typedef struct {
    char a;
    int b;
    char c;
    double d;
} NaturalStruct;

/* Pack=1 struct (no padding) */
#pragma pack(push, 1)
typedef struct {
    char a;      /* offset 0 */
    int b;       /* offset 1 */
    char c;      /* offset 5 */
    double d;    /* offset 6 */
} PackedStruct1;  /* size: 14 */
#pragma pack(pop)

/* Pack=2 struct */
#pragma pack(push, 2)
typedef struct {
    char a;      /* offset 0 */
    int b;       /* offset 2 */
    char c;      /* offset 6 */
    double d;    /* offset 8 */
} PackedStruct2;  /* size: 16 */
#pragma pack(pop)

/* Pack=4 struct */
#pragma pack(push, 4)
typedef struct {
    char a;      /* offset 0 */
    int b;       /* offset 4 */
    char c;      /* offset 8 */
    double d;    /* offset 12 */
} PackedStruct4;  /* size: 20 */
#pragma pack(pop)

/* Nested packed struct */
#pragma pack(push, 1)
typedef struct {
    char tag;
    PackedStruct1 data;
    int flags;
} NestedPacked;  /* size: 19 */
#pragma pack(pop)

/* ==========================================================================
 * Test Functions
 * ==========================================================================*/

/* Get sizes for verification */
int test_packed_sizeof_natural(void);
int test_packed_sizeof_pack1(void);
int test_packed_sizeof_pack2(void);
int test_packed_sizeof_pack4(void);
int test_nested_packed_sizeof(void);

/* Pack=1 struct operations */
PackedStruct1 test_packed1_create(char a, int b, char c, double d);
char test_packed1_get_a(PackedStruct1* s);
int test_packed1_get_b(PackedStruct1* s);
char test_packed1_get_c(PackedStruct1* s);
double test_packed1_get_d(PackedStruct1* s);
void test_packed1_set_a(PackedStruct1* s, char val);
void test_packed1_set_b(PackedStruct1* s, int val);
void test_packed1_set_c(PackedStruct1* s, char val);
void test_packed1_set_d(PackedStruct1* s, double val);
double test_packed1_compute(PackedStruct1* s);

/* Pack=2 struct operations */
void test_packed2_fill(PackedStruct2* s, char a, int b, char c, double d);
double test_packed2_compute(PackedStruct2* s);

/* Pack=4 struct operations */
void test_packed4_fill(PackedStruct4* s, char a, int b, char c, double d);
double test_packed4_compute(PackedStruct4* s);

/* Array operations */
void test_packed1_array_fill(PackedStruct1* arr, int n);
double test_packed1_array_sum(PackedStruct1* arr, int n);

/* Nested struct operations */
void test_nested_packed_fill(NestedPacked* np, char tag, char a, int b, char c, double d, int flags);
char test_nested_packed_get_tag(NestedPacked* np);
int test_nested_packed_get_flags(NestedPacked* np);
double test_nested_packed_get_data_d(NestedPacked* np);

#endif /* PACKED_STRUCTS_H */
