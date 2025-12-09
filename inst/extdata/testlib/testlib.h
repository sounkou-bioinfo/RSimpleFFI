#ifndef TESTLIB_H
#define TESTLIB_H

/* Simple Point structure */
typedef struct {
    int x;
    int y;
} Point;

/* Rectangle with nested Point */
typedef struct {
    int width;
    int height;
    Point origin;
} Rectangle;

/* Struct with bitfields - the key test case for API mode! */
typedef struct {
    unsigned int enabled : 1;
    unsigned int readonly : 1;
    unsigned int mode : 3;
    unsigned int priority : 4;
    unsigned int reserved : 23;
    int count;  /* Regular field after bitfields */
} Config;

/* Mixed struct with pointers, bitfields, and regular members */
typedef struct {
    char *buffer;
    size_t size;
    unsigned int is_open : 1;
    unsigned int is_eof : 1;
    unsigned int has_error : 1;
    unsigned int reserved : 29;
    int fd;
} FileHandle;

/* Function prototypes */
int point_distance_squared(Point *p);
int rect_area(Rectangle *r);
int config_is_valid(Config *c);
int file_is_readable(FileHandle *f);

/* Constructor functions */
Point* create_point(int x, int y);
Rectangle* create_rectangle(int w, int h, int ox, int oy);
Config* create_config(int enabled, int mode, int priority);
FileHandle* create_file_handle(const char *path);

/* Cleanup */
void destroy_point(Point *p);
void destroy_rectangle(Rectangle *r);
void destroy_config(Config *c);
void destroy_file_handle(FileHandle *f);

#endif /* TESTLIB_H */
