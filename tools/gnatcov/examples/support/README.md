Example support directory
=========================

This directory features a number of facilities to help automated build,
execution and cov analysis of simple examples for several possible Qemu
targets.

It also provides an internal "support" library, which offers a custom
exception last-chance handler to all the examples and testsuites. The common
last chance handler always outputs the same message and terminates the
program. This...

* facilitates the execution of automated tests in batch mode (compared, for
  example, to the handler in some Ravenscar libraries which just leave the
  program running idle forever), and

* provides an easy way to identify abornmal termination, with a stable output
  whatever the RTS in use.


General organization
--------------------

The facilities in this directory include a number of GNAT project files and
Makefiles:

* `common.gpr`: project file with common definitions for...

* `libsupport.gpr`: project file for both build and use of the support library
  itself and

* `base.gpr`: project file to be extended by each example project file, to
  inherit a number of common attributes.

The general GPR hierarchy is as follows:

               :   common.gpr
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
