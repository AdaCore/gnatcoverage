GNATcoverage testsuite
======================

Contents
--------

This is a Python driven testsuite for GNATcoverage.

Most tests use gprbuild to build programs for run/analysis by gnatcov/gnatemu.

The testsuite driver is `<topdir>/testsuite.py`. It scans the following
subdirectories for `test.py` instances and runs every one it finds after
switching to where it was found:

* `Qualif/` (DO-178 qualification testsuite)
* `../extra/tests/` (other testcases)


Prerequisites
-------------

You need, on your `PATH`:

* The gnatcov binary that you wish to exercise

* gprbuild >= 1.3.0

* The [e3-core](https://github.com/AdaCore/e3-core/) and
  [e3-testsuite](https://github.com/adacore/e3-testsuite/) Python packages.

* The GNAT toolchain that you wish be used to compile the tests (recent enough
  wrt `-fpreserve-control-flow`).

* The `<target>-gnatemu` binaries you wish to exercise if you are willing to
  run tests for a cross target.

Native runs are supported on x86 and x86_64-linux, using an instrumented
version of Valgrind. An instrumented Valgrind is bundled in recent binary
packages of GNATcoverage. If you are building from sources, you need to
install the GNATcoverage trace adapter into your Valgrind distrib. See the
Makefiles for this particular purpose.

If you are running tests for a ZFP or Ravenscar configuration (not a full
runtime), you also need at the toplevel, together with this README: a `support`
subdirectory corresponding to the one in the gnatcov examples.

If you retrieved this testsuite subdir as part of a full GNATcoverage tree, you
have the gnatcov sources in `../tools` and may for example do:

```shell
ln -s ../tools/gnatcov/examples/support support
```

Otherwise, you might just checkout the relevant subdir from the scm repo, for
example with:

```shell
svn co svn+ssh://$OPENDO_ROOT/tools/gnatcov/examples/support
```


Running the tests
-----------------

Launching tests is then achieved by running the top-level Python driver,
typically like:

```shell
./testsuite.py [--target=<target-product> --RTS=<rts>] [other-options] [regexp]
```

Note the use of target _product_ names here, not GCC target triplets. For
example:

```shell
# Cross configurations, using GNATemulator:
./testsuite.py --target=ppc-elf  --RTS=powerpc-elf/ravenscar-sfp-prep
./testsuite.py --target=leon-elf --RTS=ravenscar
./testsuite.py --target=ppc-vx6  --RTS=kernel --kernel=tests/kernels/vx6.8-fastsbc

# Native configuration, using an instrumented valgrind (x86-linux or x86_64-linux):
./testsuite.py
```

The optional `regexp` at the end instructs the driver to run only those tests
whose relative path from the root to `test.py` matches the expression.  If it
designates a directly reachable path (possibly expressed as relative from
`testsuite.py`), tests are searched only from there. The complete tree is
searched otherwise.

For more details on the available options:

```shell
./testsuite.py --help
```


Output logs
-----------

After test runs, each test directory contains:

* `test.py.log`, a list of all the commands executed by the test.
* `test.py.out`, all the produced regular outputs.
* `test.py.err`, all the produced error logs.

The toplevel directory contains res_gnatcov and rep_gnatcov synthesis files in
addition.

For tests using the basic Python API to gprbuild and gnatcov (the SUITE module
sketched below), the error log most of the time includes short notifications
entirely dependant on the test together with an exception traceback.

For tests using the testsuite high-level source coverage facilities, the logs
are typically much longer and structured in a systematic fashion.

Each testcase dumps a section per test driver, with a general header, and one
subsection per relevant output format (`=xcov` or `=report`, typically) which
includes the analysis of differences between expected results and the output
for this specific format.


General python support architecture
-----------------------------------

The toplevel driver is `testsuite.py`. It spawns independant instances of
`test.py`:

    testsuite.py ---o-spawn-> subdir1/test.py
                    |
                    o-spawn-> subdir2/test.py

We provide a few modules of facilities to help test writers and allow sharing
pickled data between tests and the toplevel driver, required for the production
of qualification "test-results" reports. Below is a rough sketch of the overall
architecture:

Modules are split in two packages:

* `SUITE` - set of modules relevant across the entire set of tests
* `SCOV`  - set of modules relevant for source coverage tests in particular

The split in modules is designed to isolate ~standalone functional facilities
and cut harmful dependencies.

Each test relies on a single `Test` object, which holds and controls the test
execution context (switch to initial test directory, parse command line
options, maintain execution status, ...). This facility is offered by the
`SUITE.context module`, which instanciates the object, and needs to be imported
only in a `test.py` context. Any dependency on the "current test context"
module from the toplevel driver makes no sense at all and would most probably
cause a testsuite failure.

This settled, we have:

```
SUITE
-----
 cutils.py  : Common utilities, for both tests and the toplevel driver
              [e.g. to_list(blob), contents_of(filename), ...]

 context.py : Current test context [Test class, "thistest" instance]

 tutils.py  : Common utilities that depend on "thistest" instance, so
              for tests only [xrun, gprbuild, ???]

 qdata.py   : Qualification reports facilities, to allow queuing
              pickled data between tests and the toplevel driver

 gprutils.py: Facilities to help the development of testcases
              exercising gnatcov's GPR support.

SCOV
----
 tc.py      : TestCase class for source coverage tests

 htc.py     : HarnessTestCase class for harness self tests

 report.py  : Format checker for qualified reports,

 map.py     : MapChecker class for gnatcov map-routines tests

 internals
 ---------
   cnotes.py    : Classes to represent expected or emitted coverage notes

   driver.py    : Class to help drive a source coverage test [SCOV_helper]

   *expanders.py : Expand coverage note instances from gnatcov reports or from
                   expectation-specs + sources

   xnotep.py    : Class to represent user expectation patterns

   segments.py :  Source segment abstractions [line, line:col, ...], to
                  help check for inclusions.

   tfiles.py :    Text files (and lines) abstractions to automate processing
                  on read and assign numbers to lines.
```
