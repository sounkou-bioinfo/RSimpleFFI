# Get compiler from R configuration
R := $(shell which R)
CC := $(shell "$(R)" CMD config CC | awk '{print $$1}')
CFLAGS = -Wall -fPIC -O2
LDFLAGS = -shared

LIBNAME = libtestlib.so
SOURCES = testlib.c
OBJECTS = $(SOURCES:.c=.o)
HEADER = testlib.h

all: $(LIBNAME)

$(LIBNAME): $(OBJECTS)
	$(CC) $(LDFLAGS) -o $@ $^

%.o: %.c $(HEADER)
	$(CC) $(CFLAGS) -c $< -o $@

clean:
	rm -f $(OBJECTS) $(LIBNAME)

install: $(LIBNAME)
	@echo "Library built successfully: $(LIBNAME)"

.PHONY: all clean install
