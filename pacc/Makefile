PROGRAMS = pacc

# nclude ../src/runtime/runtime.mk

SRCS-pacc = $(CURDIR)/debug.c $(CURDIR)/lex.c $(CURDIR)/parse.c

CFLAGS+=-I$(SRCDIR)/runtime \
	-I$(SRCDIR)/unix_process \
	-I$(SRCDIR)/x86_64

all: $(PROGRAMS)

include ../rules.mk
