Example support directory
=========================

This directory features a number of facilities to help automated build,
execution and cov analysis of simple examples for several possible Qemu
targets.

It provides an internal "support" library, which offers

* A set of common helpers for test programs, such as an "Assert" procedure
  and a set of "Identity" functions, some of which implemented in Ada and
  exposed to C with straight symbol names to accommodate legacy uses.

* For cross configurations, a custom exception last-chance handler to all
  the examples and testsuites, which always outputs the same message and
  terminates the program.

  This facilitates the execution of automated tests in batch mode (compared,
  for example, to the handler in some Ravenscar libraries which just leave the
  program running idle forever), and provides an easy way to identify abornmal
  termination, with a stable output whatever the RTS in use.

* For cross configurations, a set of common memory oriented subprograms to
  compensate their possible absence in some environments with very restricted
  GNAT runtimes.

The directory also offers, through an separate project file, an alernative
last_chance_handler which terminates a program silently without signalling an
error condition. This is useful for tests intended to verify the behavior
of the tool in situations where an unhandled exception occurs.

This separate project is provided aside from the support library as a
non-library project to make sure that the alternative last chance
handler symbol gets incorporated in the link closure reliably with a
standalone object file.

Both projects are built once early and then reused as Externally_Built
afterwards, possibly concurrently when the testsuite executes tests in
parallel.

General organization
--------------------

The facilities in this directory include a number of GNAT project files and
Makefiles:

* `common.gpr`: project file with common definitions

* `libsupport.gpr`: project file for both build and use of the support library
  itself and

* `base.gpr`: project file to be extended by each example project file, to
  inherit a number of common attributes.

Configuration parameters are latched in a `conf.gpr` project file at
build time, allowing use without having to re-specify all the values
through scenario variables afterwards. `conf.gpr` is generated from a
`conf-template.gpr` file by substituting patterns with actual values.

The general GPR hierarchy is as follows:

               :   common.gpr
               :   conf.gpr  <-- conf-template.gpr
               :      w  w
               :      |  |
     support/  :      | libsupport.gpr (With common)
               :      |  w
               :      |  |
               :    base.gpr (with common, libsupport)
                      x
                      |
    <example>/ :   example.gpr (eXtends base)

This is ~mirrored in the Makefile organization, with `i` to denote
inclusions:

               :    Makefile.common
               :      i         i
     support/  :      |         |
               :  .examples   .libsupport
                     i
                     |
    <example>/ :  Makefile


--board vs --target use by the testsuite
----------------------------------------

     ./testsuite.py --target=<t> [--board=<b>]

        make -f Makefile.libsupport TARGET=<t> [BOARD=<t>]

        spawn (test.py)
          |
          v
     .../test.py

        gnatcov run --target=[<b> if --board=specified at toplevel]
                             <t> otherwise

        gprbuild -XTARGET=<t> [-XBOARD=<b>]
