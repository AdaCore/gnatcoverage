This directory contains a few examples illustrating possible ways to build, run
and do coverage analysis with GNAT Pro and GNATcoverage

Except `support`, each subdirectory hosts an example and features a Makefile
that allows launching a complete build/run/analyze sequence for a variety of
targets, assuming you have the corresponding GNAT & GNATemulator products
installed.

The `support` subdirectory is common to all the other ones, and additional
information about the general Makefile and project files structure is available
in a dedicated README file there.

Unless specified otherwise in the specific example Makefile (a couple of
examples can only run on specific targets), you may select the following target
to use by setting the `TARGET` and `RTS` variables explicitly:

```shell
make TARGET=powerpc-elf RTS=zfp-mpc8641   runs on bare PowerPC
make TARGET=powerpc-eabispe RTS=zfp-p2020 runs on bare PowerPC/e500v2
make TARGET=leon-elf RTS=zfp-leon         runs on bare LEON2
```

as well as:

```shell
make
```

for native `x86-linux/windows` or `x86_64-linux/windows` runs relying on a
specially instrumented version of Valgrind or DynamoRIO.
