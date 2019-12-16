Package contents
================

This package provides the gnatcov front-end to coverage analysis activities.
This front-end exposes two major classes of services:

* a wrapper to an instrumented execution environment able to produce execution
  traces (`gnatcov run`), and

* a trace analyzer able to render coverage results in various output formats
  (`gnatcov coverage`).

Instrumented execution environments are provided as part of the GNATemulator
separate product line for cross configurations. For native x86 or x86_64-linux
configurations, we rely on a Valgrind plugin to produce execution traces. For
native x86 or x86_64-windows configurations, we rely on a DynamoRIO plugin
instead.

Documentation, including a getting started guide, is available from the
"GNATcoverage User's Guide" in the `share/doc/gnatcoverage` sub-directory of
this distribution.

This package also provides examples with Makefiles that automate the full
build/run/analyze sequence on sample programs.

The "starter" example is a very simple case meant as a quick-starter.  You can
exercise it by just switching to share/examples/gnatcoverage/starter and
typing:

```shell
engines $ make [TARGET=powerpc-elf] [RTS=zfp-prep]
```

The local Makefile includes a generic Makefile common to all the examples, and
you may run & try the other ones similarily.


Build from sources - quick example for an intel-linux host
----------------------------------------------------------

Prerequisite: GNU make, a recent Ada2012 capable GNAT compiler, a `prefix`
environment variable designating the GNAT installation root.

1.  Check if your GNAT installation includes the GNATcoll components for mmap
    and project files management, materialized by `gnatcoll-mmap.ali` and
    `gnatcoll-projects.ali` files in the installation tree rooted at `$prefix`.

    If it doesn't, build and install the components from sources available from
    https://github.com/AdaCore/gnatcoll-core.

    ```shell
    gnatcoll-core $ make setup ENABLE_SHARED=no GNATCOLL_MMAP=yes prefix=$prefix

    gnatcoll-core $ make
    gnatcoll-core $ make install
    ```

2.  Build libopcodes, libbfd and libiberty libraries from Binutils, which
    gnatcov needs to support object code disassembling on all platforms.

    Best is to use a Binutils release for this, at least 2.29 which
    is known to work.

    ```shell
    binutils-2.29 $ ./configure --enable-targets=all \
                         --disable-werror --without-zlib --disable-gdb \
                         --disable-sim --disable-ld --disable-libquadmath \
                         --disable-readline
    binutils-2.29 $ make
    ```

    These libraries are statically linked, so installation is not required: the
    gnatcov build process only needs access to the source+build directory (no
    need to run `make install`).

3.  Build & install Libadalang: please refer to
    [Libadalang's instructions](https://github.com/adacore/libadalang#quick-guide-to-use-libadalang).

4.  Build & install a base Valgrind:

    Best is to use a Valgrind release for this, for example 3.10.1 or
    3.13 which are known to work.

    ```shell
    valgrind-3.13 $ ./configure --prefix=$prefix
    valgrind-3.13 $ make install
    ```

5.  Build & install the gnatcov front-end per se, plus the Valgrind trace
    plugin:

    ```shell
    gnatcoverage/tools/gnatcov $ make bin adapters VALGRIND_PREFIX=$prefix \
                                      BINUTILS_SRC_DIR=<path-to-binutils-src>
    gnatcoverage/tools/gnatcov $ make PREFIX=$prefix VALGRIND_PREFIX=$prefix \
                                      install-bin install-examples \
                                      install-adapters
    ```
