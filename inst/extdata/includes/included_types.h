#ifndef INCLUDED_TYPES_H
#define INCLUDED_TYPES_H

/* Header file to be included by other headers */

#define BUFFER_SIZE 4096
#define TIMEOUT_MS 1000

typedef struct {
    long timestamp;
    int event_type;
} Event;

typedef struct {
    char *data;
    size_t length;
    size_t capacity;
} Buffer;

// Function declarations
Event create_event(int type);
Buffer* allocate_buffer(size_t size);
void free_buffer(Buffer *buf);

#endif
