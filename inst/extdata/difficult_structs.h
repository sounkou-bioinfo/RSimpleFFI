#ifndef DIFFICULT_STRUCTS_H
#define DIFFICULT_STRUCTS_H

#include <stdio.h>

/* Test FILE* and other opaque pointers */
FILE* open_file(const char* path, const char* mode);
int close_file(FILE* fp);
int write_to_file(FILE* fp, const char* data);

/* Forward declarations and recursive structs */
struct Node;

struct Node {
    int value;
    struct Node* next;
    struct Node* prev;
};

struct Tree {
    int data;
    struct Tree* left;
    struct Tree* right;
    struct Tree* parent;
};

/* Nested structs - multiple levels */
struct Inner {
    int x;
    int y;
};

struct Middle {
    struct Inner inner;
    double scale;
};

struct Outer {
    struct Middle middle;
    char label[32];
    int count;
};

/* Struct with function pointers */
typedef int (*Comparator)(const void* a, const void* b);
typedef void (*Callback)(int status, void* user_data);

struct Handler {
    Comparator compare;
    Callback notify;
    void* context;
};

/* Struct with arrays of structs */
struct Point2D {
    int x;
    int y;
};

struct Polygon {
    struct Point2D vertices[10];
    int num_vertices;
};

/* Struct with pointer to array vs array of pointers */
struct BufferInfo {
    int* data;           // pointer to array
    int** matrix;        // array of pointers (or pointer to pointer)
    int sizes[8];        // fixed array
};

/* Anonymous unions and structs (if supported) */
struct Variant {
    int type;
    union {
        int i;
        double d;
        char* s;
    } value;
};

/* Bit fields */
struct Flags {
    unsigned int flag_a : 1;
    unsigned int flag_b : 1;
    unsigned int reserved : 6;
    unsigned int mode : 8;
};

/* Const and volatile */
struct ConstData {
    const int* readonly;
    volatile int* shared;
    const volatile int* special;
};

/* Self-referential typedef */
typedef struct LinkedList LinkedList;
struct LinkedList {
    void* data;
    LinkedList* next;
};

/* Functions with difficult signatures */
struct Node* create_node(int value);
void insert_node(struct Node** head, struct Node* new_node);
struct Node* find_node(struct Node* head, int value);
void free_list(struct Node* head);

struct Tree* build_tree(int values[], int size);
void traverse_tree(struct Tree* root, void (*visitor)(int));

/* Function returning function pointer */
Comparator get_comparator(int type);

/* Function with array parameters */
void process_matrix(int rows, int cols, double matrix[rows][cols]);
void process_points(struct Point2D points[], int count);

/* Variadic functions */
int printf_like(const char* format, ...);

#endif
