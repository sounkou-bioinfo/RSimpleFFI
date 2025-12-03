#ifndef SIMPLE_TYPES_H
#define SIMPLE_TYPES_H

/* Simple test header with basic types */

#define MAX_BUFFER 1024
#define MIN_SIZE 16

// Basic struct
struct Point {
    int x;
    int y;
};

// Function declarations
int add(int a, int b);
double multiply(double x, double y);
void process_point(struct Point *p);

#endif
