#ifndef TYPEDEF_STRUCTS_H
#define TYPEDEF_STRUCTS_H

/* Test header with typedef structs */

#define VECTOR_DIM 3
#define COLOR_DEPTH 256

// typedef struct patterns
typedef struct {
    double x;
    double y;
    double z;
} Vector3D;

typedef struct {
    unsigned char r;
    unsigned char g;
    unsigned char b;
    unsigned char a;
} Color;

typedef struct {
    int width;
    int height;
} Dimensions;

// Functions using typedef'd types
Vector3D cross_product(Vector3D a, Vector3D b);
Color blend_colors(Color c1, Color c2, double alpha);
Dimensions scale_dimensions(Dimensions d, double factor);

#endif
