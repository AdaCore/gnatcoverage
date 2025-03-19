This directory contains a few examples illustrating possible ways to build, run
and do coverage analysis with GNAT Pro and GNATcoverage

Except `support` and `doc`, each subdirectory hosts an example and features a
Makefile that allows launching a complete build/run/analyze sequence for a
variety of targets, assuming you have the corresponding GNAT & GNATemulator
products installed.

The `doc` subdirectory holds the sources used in the various examples in the
user's manual. Each one also features a Makefile that replicates the commands
demonstrated in the user's manual relevant section. These examples only work
for native targets.

The `support` subdirectory is common to all the other ones, and additional
information about the general Makefile and project files structure is available
in a dedicated README file there.

Unless specified otherwise in the specific example Makefile (a couple of
examples can only run on specific targets), you may select the following target
to use by setting the `TARGET` and `RTS` variables explicitly:

```shell
make TARGET=powerpc-elf RTS=light-mpc8641   runs on bare PowerPC
make TARGET=leon3-elf RTS=light-leon3     runs on bare LEON3
```

as well as:

```shell
make
```

for native `x86-linux/windows` or `x86_64-linux/windows` runs.
