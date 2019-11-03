

PROGRAMS= \
	buffer_test \
	id_heap_test \
	memops_test \
	network_test \
	objcache_test \
	parser_test \
	pqueue_test \
	random_test \
	range_test \
	table_test \
	tuple_test \
	udp_test \
	vector_test\



all: $(PROGRAMS)

include ../../src/runtime/runtime.mk

SKIP_TEST=	network_test udp_test random_test

SRCS-buffer_test= \
	$(CURDIR)/buffer_test.c \
	$(RUNTIME)\
	$(SRCDIR)/unix_process/unix_process_runtime.c

SRCS-id_heap_test= \
	$(CURDIR)/id_heap_test.c \
	$(RUNTIME)\
	$(SRCDIR)/tfs/tfs.c \
	$(SRCDIR)/tfs/tlog.c \
	$(SRCDIR)/unix_process/unix_process_runtime.c

SRCS-memops_test= \
	$(CURDIR)/memops_test.c \
	$(RUNTIME)\
	$(SRCDIR)/unix_process/unix_process_runtime.c

SRCS-network_test= \
	$(CURDIR)/network_test.c \
	$(RUNTIME)\
	$(SRCDIR)/tfs/tfs.c \
	$(SRCDIR)/tfs/tlog.c \
	$(SRCDIR)/unix_process/unix_process_runtime.c \
	$(SRCDIR)/unix_process/mmap_heap.c \
	$(SRCDIR)/unix_process/http.c \
	$(SRCDIR)/unix_process/socket_user.c \
	$(SRCDIR)/unix_process/tiny_heap.c

SRCS-objcache_test= \
	$(CURDIR)/objcache_test.c \
	$(SRCDIR)/runtime/heap/objcache.c \
	$(RUNTIME)\
	$(SRCDIR)/tfs/tfs.c \
	$(SRCDIR)/tfs/tlog.c \
	$(SRCDIR)/unix_process/unix_process_runtime.c \
	$(SRCDIR)/unix_process/mmap_heap.c

SRCS-parser_test= \
	$(CURDIR)/parser_test.c \
	$(SRCDIR)/runtime/tuple_parser.c \
	$(RUNTIME)\
	$(SRCDIR)/unix_process/unix_process_runtime.c

SRCS-pqueue_test= \
	$(CURDIR)/pqueue_test.c \
	$(RUNTIME)\
	$(SRCDIR)/unix_process/unix_process_runtime.c

SRCS-range_test= \
	$(CURDIR)/range_test.c \
        $(RUNTIME)\
	$(SRCDIR)/runtime/crypto/chacha.c \
	$(SRCDIR)/unix_process/unix_process_runtime.c

SRCS-random_test = \
	$(CURDIR)/random_test.c \
        $(RUNTIME)\
	$(SRCDIR)/tfs/tfs.c \
	$(SRCDIR)/tfs/tlog.c \
	$(SRCDIR)/unix_process/unix_process_runtime.c

SRCS-table_test= \
	$(CURDIR)/table_test.c \
        $(RUNTIME)\
	$(SRCDIR)/unix_process/unix_process_runtime.c

SRCS-tuple_test= \
	$(CURDIR)/tuple_test.c \
        $(RUNTIME)\
	$(SRCDIR)/unix_process/unix_process_runtime.c

SRCS-udp_test= \
	$(CURDIR)/udp_test.c \
        $(RUNTIME)\
	$(SRCDIR)/tfs/tfs.c \
	$(SRCDIR)/tfs/tlog.c \
	$(SRCDIR)/unix_process/unix_process_runtime.c \
	$(SRCDIR)/unix_process/mmap_heap.c \
	$(SRCDIR)/unix_process/http.c \
	$(SRCDIR)/unix_process/socket_user.c \
	$(SRCDIR)/unix_process/tiny_heap.c

SRCS-vector_test= \
	$(CURDIR)/vector_test.c \
        $(RUNTIME)\
	$(SRCDIR)/unix_process/unix_process_runtime.c


CFLAGS+=	-I$(SRCDIR)/tfs \
		-I$(SRCDIR)/unix_process \
		-I$(SRCDIR)/unix \
		-I$(SRCDIR)/x86_64
#CFLAGS+=	-DENABLE_MSG_DEBUG -DID_HEAP_DEBUG

CLEANDIRS+=	$(OBJDIR)/test

# gcov support
CFLAGS+=	-fprofile-arcs -ftest-coverage
LDFLAGS+=	-fprofile-arcs
GCDAFILES=	$(sort $(foreach p,$(PROGRAMS),$(patsubst %.o,%.gcda,$(OBJS-$p))))
GCOVFILES=	$(sort $(foreach p,$(PROGRAMS),$(patsubst %.o,%.gcno,$(OBJS-$p))))
GCOVFILES+=	$(GCDAFILES) $(OBJDIR)/gcov-tests.info
CLEANFILES+=	$(GCOVFILES)


.PHONY: test gcov gcov-clean

test: all
	$(Q) $(RM) $(GCDAFILES)
	$(foreach p,$(filter-out $(SKIP_TEST),$(PROGRAMS)),$(call execute_command,$(PROG-$p)))

gcov: test
	$(foreach p,$(PROGRAMS),$(call execute_command,$(GCOV) -o $(OBJDIR) $(PROG-$p)))
	$(LCOV) --capture --directory $(ROOTDIR) --output-file $(OBJDIR)/gcov-tests.info
	$(MKDIR) $(OBJDIR)/gcov
	$(GENHTML) $(OBJDIR)/gcov-tests.info --output-directory $(OBJDIR)/gcov

pre-clean:
	$(Q) $(RM) -r $(OBJDIR)/gcov

gcov-clean:
	$(Q) $(RM) $(GCOVFILES)

include ../../rules.mk

ifeq ($(UNAME_s),Darwin)
CFLAGS+=	-DNO_EPOLL
endif

