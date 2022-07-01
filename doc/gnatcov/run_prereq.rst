.. _run-prereq:

Compilation prerequisites
=========================

If the traces are intended to be used for :ref:`source coverage analysis
<scov>`, regardless of the actual criteria to be assessed afterwards, sources
must be compiled with :cmd-option:`-g -fpreserve-control-flow -fdump-scos` to
control optimizations and allow mapping trace info to source constructs.
:cmd-option:`-gno-strict-dwarf` is required in addition for VxWorks targets,
together with :cmd-option:`-mlongcall` for PowerPC configurations.
Optimization is supported up to :cmd-option:`-O1`, with inlining allowed.

The :cmd-option:`-fprofile-arcs` or :cmd-option:`--coverage` compilation
switches, intended for analysis with the gcov tool, are incompatible with the
needs of |gcp| for binary traces and must not be used. Likewise for the
:cmd-option:`-gnatV` family of GNAT Pro switches requesting data validity
checks other than those mandated by the Ada RM (special case of
:cmd-option:`-gnatVd`).

For object coverage assessments, only :cmd-option:`-g` is of possible use, if
you intend to produce annotated sources as coverage reports. If source coverage
analysis is to be performed *as well*, and you have no particular constraint on
the form of the machine code for which object coverage is needed, the whole
process might turn simpler if the same compilation options are used for both
kinds of analysis. There is no incompatibility with object coverage analysis
incurred by compiling the code compiled with options required for source
coverage.
