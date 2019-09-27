PROGRAMS = pacc

include ../src/runtime/runtime.mk

SRCS-pacc = $(CURDIR)/debug.c  $(CURDIR)/main.c $(CURDIR)/lex.c $(CURDIR)/parse.c $(RUNTIME) $(SRCDIR)/unix_process/unix_process_runtime.c

CFLAGS+=-I$(SRCDIR)/unix_process \
	-I$(SRCDIR)/x86_64\
        -I.

all: $(PROGRAMS)

include ../rules.mk
