OUTPUT=$(PWD)/../output/xhyve
TARGET=userboot_nanos.so

include ../rules.mk

SRC=$(PWD)/../src

INCLUDES = -I$(SRC)/x86_64 -I$(SRC)/runtime -I$(SRC)/unix_process -I$(OUTPUT) -I$(SRC)/tfs

PROGRAMS=$(TARGET)

SRCS-$(TARGET)= $(SRC)/runtime/bitmap.c \
	$(SRC)/runtime/buffer.c \
	$(SRC)/runtime/extra_prints.c \
	$(SRC)/runtime/format.c \
	$(SRC)/runtime/heap/freelist.c \
	$(SRC)/runtime/heap/id.c \
	$(SRC)/runtime/heap/mcache.c \
	$(SRC)/runtime/heap/objcache.c \
	$(SRC)/runtime/memops.c \
	$(SRC)/runtime/merge.c \
	$(SRC)/runtime/pqueue.c \
	$(SRC)/runtime/random.c \
	$(SRC)/runtime/range.c \
	$(SRC)/runtime/runtime_init.c \
	$(SRC)/runtime/sha256.c \
	$(SRC)/runtime/symbol.c \
	$(SRC)/runtime/table.c \
	$(SRC)/runtime/timer.c \
	$(SRC)/runtime/tuple.c \
	$(SRC)/runtime/string.c \
	$(SRC)/runtime/crypto/chacha.c \
	$(SRC)/tfs/tfs.c \
	$(SRC)/tfs/tlog.c 

LDFLAGS += -bundle 
$(TARGET): t.c $(OBJDIR)/closure_templates.h
	cc -fPIC -I. $(INCLUDES) -bundle -o $(TARGET) t.c

run: $(TARGET)
	xhyve -f fbsd,$(TARGET),$(SRC)/../output/image/disk.raw,""
