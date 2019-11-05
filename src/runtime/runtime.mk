CFLAGS += -I$(SRCDIR)/runtime

RUNTIME = $(SRCDIR)/runtime/bitmap.c \
	$(SRCDIR)/runtime/buffer.c \
	$(SRCDIR)/runtime/crypto/chacha.c \
	$(SRCDIR)/runtime/extra_prints.c \
	$(SRCDIR)/runtime/format.c \
	$(SRCDIR)/runtime/heap/id.c \
	$(SRCDIR)/runtime/heap/mcache.c \
	$(SRCDIR)/runtime/heap/objcache.c \
	$(SRCDIR)/runtime/memops.c \
	$(SRCDIR)/runtime/merge.c \
	$(SRCDIR)/runtime/method.c \
	$(SRCDIR)/runtime/pqueue.c \
	$(SRCDIR)/runtime/random.c \
	$(SRCDIR)/runtime/range.c \
	$(SRCDIR)/runtime/runtime_init.c \
	$(SRCDIR)/runtime/sha256.c \
	$(SRCDIR)/runtime/string.c \
	$(SRCDIR)/runtime/symbol.c \
	$(SRCDIR)/runtime/table.c \
	$(SRCDIR)/runtime/timer.c \
	$(SRCDIR)/runtime/tuple.c \
	$(SRCDIR)/runtime/tuple_parser.c \
	$(SRCDIR)/runtime/vector.c
