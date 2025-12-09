#include <stdlib.h>
#include <string.h>
#include "testlib.h"

/* Point functions */
int point_distance_squared(Point *p) {
    if (!p) return -1;
    return p->x * p->x + p->y * p->y;
}

Point* create_point(int x, int y) {
    Point *p = (Point*)malloc(sizeof(Point));
    if (p) {
        p->x = x;
        p->y = y;
    }
    return p;
}

void destroy_point(Point *p) {
    free(p);
}

/* Rectangle functions */
int rect_area(Rectangle *r) {
    if (!r) return -1;
    return r->width * r->height;
}

Rectangle* create_rectangle(int w, int h, int ox, int oy) {
    Rectangle *r = (Rectangle*)malloc(sizeof(Rectangle));
    if (r) {
        r->width = w;
        r->height = h;
        r->origin.x = ox;
        r->origin.y = oy;
    }
    return r;
}

void destroy_rectangle(Rectangle *r) {
    free(r);
}

/* Config functions - bitfield handling */
int config_is_valid(Config *c) {
    if (!c) return 0;
    return c->enabled && c->priority > 0;
}

Config* create_config(int enabled, int mode, int priority) {
    Config *c = (Config*)malloc(sizeof(Config));
    if (c) {
        memset(c, 0, sizeof(Config));
        c->enabled = enabled ? 1 : 0;
        c->mode = mode & 0x7;  /* 3 bits */
        c->priority = priority & 0xF;  /* 4 bits */
        c->count = 0;
    }
    return c;
}

void destroy_config(Config *c) {
    free(c);
}

/* FileHandle functions - complex mixed struct */
int file_is_readable(FileHandle *f) {
    if (!f) return 0;
    return f->is_open && !f->has_error;
}

FileHandle* create_file_handle(const char *path) {
    FileHandle *f = (FileHandle*)malloc(sizeof(FileHandle));
    if (f) {
        memset(f, 0, sizeof(FileHandle));
        f->buffer = path ? strdup(path) : NULL;
        f->size = path ? strlen(path) : 0;
        f->is_open = 1;
        f->is_eof = 0;
        f->has_error = 0;
        f->fd = -1;
    }
    return f;
}

void destroy_file_handle(FileHandle *f) {
    if (f) {
        free(f->buffer);
        free(f);
    }
}
