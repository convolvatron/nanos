# nanos

[![CircleCI](https://circleci.com/gh/nanovms/nanos.svg?style=svg)](https://circleci.com/gh/nanovms/nanos)

### Building/Running

Please use [https://github.com/nanovms/ops](ops) unless you are planning
on modifying nanos.

For Nanos try running the first example first:
```
make run-nokvm
```

To try a different target currently found in examples/ you can:

1) cp the manifest file to target.manifest
2) add your code and set a target in examples/Makefile

```
TARGET=mynewtarget make run-novkm
```

### Creating a Manifest

* arguments
* environment variables

### TFS

TFS is the current filesystem utilized by Nanos.

### Optional Flags

* thread tracing

```
futex_trace: t
```

* syscall tracing

```
debugsyscalls: t
```

* stackdump

```
fault: t
```

[https://github.com/nanovms/nanos/wiki/Architecture](Architecture)

[https://github.com/nanovms/nanos/wiki/debugging](Debugging Help)

[https://github.com/nanovms/nanos/wiki/networking-setup](Manual Networking Setup)

[Build Envs](https://github.com/nanovms/nanos/wiki/Build-Envs)

[Reference Materials](https://github.com/nanovms/nanos/wiki/reference-materials)
