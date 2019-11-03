PROGRAMS=	mkfs dump

include ../src/runtime/runtime.mk

SRCS-mkfs= \
	$(CURDIR)/mkfs.c \
	$(RUNTIME)\
	$(SRCDIR)/tfs/tfs.c \
	$(SRCDIR)/tfs/tlog.c \
	$(SRCDIR)/unix_process/unix_process_runtime.c

SRCS-dump= \
	$(CURDIR)/dump.c \
	$(RUNTIME)\
	$(SRCDIR)/tfs/tfs.c \
	$(SRCDIR)/tfs/tlog.c \
	$(SRCDIR)/unix_process/unix_process_runtime.c

CFLAGS+=-I$(SRCDIR)/runtime \
	-I$(SRCDIR)/tfs \
	-I$(SRCDIR)/unix_process \
	-I$(SRCDIR)/x86_64

all: $(PROGRAMS)

include ../rules.mk
