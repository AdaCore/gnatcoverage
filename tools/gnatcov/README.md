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
build/run/analyze sequence on sample programs targeted to GNATemulator.

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

Prerequisite: GNU make, a recent Ada2012 capable GNAT compiler.

1. Build & install the GNATcoll components on top of the GNAT installation,
   assumed to be in a directory designated by a `prefix` environment variable.

    gnatcov relies on the "GNAT project file" support" of GNATcoll, so
    at least this needs to be enabled:

    ```
    gnatcoll-src $ ./configure  --disable-shared --prefix=$prefix \
                     --without-python --without-iconv --enable-projects \
                     --without-sqlite --disable-syslog --without-gmp
    gnatcoll-src $ make
    gnatcoll-src $ make install
    ```

2. Build various libraries from Binutils/GDB:

    To support object code disassembling on all platforms, gnatcov relies on
    Binutils's libopcodes, libbfd and libiberty. In order to build these
    libraries, run:

    ```shell
    binutils-gdb-src $ ./configure --enable-targets=all \
                         --disable-werror --without-zlib --disable-gdb \
                         --disable-sim --disable-ld --disable-libquadmath \
                         --disable-readline
    binutils-gdb-src $ make
    ```

    These libraries are statically linked, so installation is not required: the
    gnatcov build process only needs access to the source+build directory (no
    need to run `make install`).

3. Build & install a base Valgrind:

    ```shell
    valgrind-3.10.1 $ ./configure --prefix=$prefix
    valgrind-3.10.1 $ make install
    ```

4. Build & install the gnatcov front-end per se, plus the Valgrind trace
   plugin:

    ```shell
    couverture.git/tools/gnatcov $ make bin adapters \
                                        BINUTILS_BUILD_DIR=$binutils-gdb-src
    couverture.git/tools/gnatcov $ make PREFIX=$prefix VALGRIND_PREFIX=$prefix \
                                        install-bin install-examples \
                                        install-adapters
    ```
