#ifndef WITH_INCLUDES_H
#define WITH_INCLUDES_H

#include "included_types.h"
#include <stdint.h>
#include <stdio.h>

/* Header that includes other headers */

#define MAX_EVENTS 100

// Struct using types from included files
struct EventQueue {
    Event events[MAX_EVENTS];
    Buffer *buffer;
    uint32_t count;
    FILE *log_file;
};

// Functions using types from includes
int process_queue(struct EventQueue *queue);
Event read_event_from_buffer(Buffer *buf);
void log_event(FILE *fp, Event evt);
uint32_t get_queue_size(struct EventQueue *queue);

#endif
