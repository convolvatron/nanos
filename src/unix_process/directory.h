#include <sys/types.h>
#include <dirent.h>

            
#define foreach_dir(__i, __p)
for(DIR *__d = opendir(__p); __d; __d = 0)
    for(struct dirent *__e; __e = readdir(__d);)
        for(char *__i = __e->d_name; __i; __i =0)
