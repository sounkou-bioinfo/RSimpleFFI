# Get compiler from R configuration. R CMD check puts a guard script named
# `R` on PATH, so tests can provide the real executable via R_BIN.
R := $(if $(R_BIN),$(R_BIN),$(shell which R))
CC := $(shell "$(R)" CMD config CC | awk '{print $$1}')
SHLIB_EXT := $(shell "$(R)" CMD config SHLIB_EXT)
CFLAGS = -Wall -fPIC -O2
LDFLAGS = -shared

LIBNAME = libtestlib$(SHLIB_EXT)
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
