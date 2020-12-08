Package contents
================

This package provides the gnatcov front-end to coverage analysis activities.
This front-end exposes two major classes of services:

* A wrapper to an instrumented execution environment able to produce execution
  traces (`gnatcov run`),

* A source instrumenter for configurations where such an execution environment
  is not available or where source instrumentation is a better option anyway,

* A trace analyzer able to render coverage results in various output formats
  (`gnatcov coverage`), from traces obtained either from `gnatcov run` or
  from the execution of an instrumented version of the program.

Instrumented execution environments are provided as part of the GNATemulator
separate product line for cross configurations.

For native `x86/x86_64-linux/windows` configurations, we now only support the
source instrumentation based mode.

Documentation, including a getting started guide, is available from the
"GNATcoverage User's Guide" in the `share/doc/gnatcoverage` sub-directory of
this distribution.

This package also provides examples with Makefiles that automate the full
build/run/analyze sequence on sample programs.

The "starter" example is a very simple case meant as a quick-starter.  You can
exercise it by just switching to share/examples/gnatcoverage/starter and
typing:

```shell
engines $ make [TARGET=powerpc-elf] [RTS=zfp-mpc8641]
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

    Best is to use a Binutils release for this, such as 2.35 which
    is known to work.

    ```shell
    binutils-2.35 $ ./configure --enable-targets=all \
                         --disable-werror --disable-gdb \
                         --disable-sim --disable-ld --disable-libquadmath \
                         --disable-readline
    binutils-2.35 $ make
    ```

    These libraries are statically linked, so installation is not required: the
    gnatcov build process only needs access to the source+build directory (no
    need to run `make install`).

3.  Build or install Libadalang: please refer to
    [Libadalang's instructions](https://github.com/adacore/libadalang#quick-guide-to-use-libadalang).
    Note that when building GNATcoverage's `master` branch, you should use
    Libadalang and Langkit's `stable` branches as well.

4.  If your GNAT toolchain does not come with a `gnat_util` library
    (search for `gnat_util.gpr`), create one within the gnatcov source tree
    from a recent GNAT Community source package.

    ```shell
    gnatcoverage/tools/gnatcov $
      make gnat_util GNAT_SRC_DIR=<gnat-community-source-pkg>/src/ada
    ```

5.  Build & install the gnatcov front-end per se.

    ```shell
    gnatcoverage/tools/gnatcov $
       make BINUTILS_SRC_DIR=<path-to-binutils-src> bin
       make PREFIX=$prefix install-bin install-examples
    ```
