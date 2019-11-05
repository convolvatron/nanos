#include <runtime.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>
#include <pacc.h>

buffer read_file(heap h, string filename)
{
    buffer tmpbuf = little_stack_buffer(buffer_length(filename) + 1);
    struct stat st;
    char *n = cstring(filename, tmpbuf);
    if (stat(n, &st) < 0) {
        halt("no such file %b", filename);
    }
    int len = st.st_size;
    int fd = open(n, O_RDONLY);
    if (fd < 0) {
        halt("couldn't open file %b", filename);
    }
    buffer out = allocate_buffer(h, allocate(h, len), h, len);
    int r = read(fd, out->contents, len);
    if (r != len) {
        halt("short read %b", filename);
    }
    return out;
}

int main(int argc, char **argv)
{
    heap h = init_process_runtime();
    for (int i = 1 ;i <argc; i++ ){
        // shuld be shared tree
        rprintf ("%v", parse_init(h, read_file(h, wrap_buffer_cstring(h, argv[i]))));
    }
}

