.. _run-prereq:

Compilation prerequisites
=========================

General rules
-------------

If the traces are intended to be used for :ref:`source coverage analysis
<scov>`, regardless of the actual criteria to be assessed afterwards, sources
must be compiled with :cmd-option:`-g -fpreserve-control-flow -fdump-scos` to
control optimizations and allow mapping trace info to source constructs.
:cmd-option:`-gno-strict-dwarf` is required in addition for VxWorks targets,
together with :cmd-option:`-mlongcall` for PowerPC configurations.

Optimization is supported up to :cmd-option:`-O1`, with inlining
allowed. However, if the binary version of the code under test during
the coverage campaign is intended to be used directly in operational
conditions, care is needed to prevent inlining of the code under test
within the testing code at compilation time in unit-testing campaigns.
With GCC based toolchains such as GNAT Pro, this can be achieved by
adding :cmd-option:`-fno-inline` to the compilation options of the
testing code.

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

Consolidation related rules
---------------------------

For object or source level criteria, |gcv| computes the coverage achieved for
the full set of routines or source units declared to be of interest amongst
those exposed by the union of the exercised executables, as designated by the
set of consolidated traces;

For the purpose of computing combined coverage achievements, two symbols are
considered overlapping when all the following conditions are met:

* Both symbols have identical names at the object level,

* Both symbols have DWARF debug information attached to them,

* According to this debug information, both symbols originate from the same
  compilation unit, denoted by the full path of the corresponding source file.

By this construction, a symbol missing debug information is never considered
overlapping with any other symbol. Whatever coverage is achieved on such a
symbol never gets combined with anything else and the only kind of report where
the symbol coverage is exposed is the :cmd-option:`=asm` assembly output for
object level criteria.

Moreover, for object level coverage criteria, |gcvcov| will issue a
consolidation error when two symbols are found to overlap but have
structurally different machine code, which happens for example when the same
unit is compiled with different different optimization levels for
different executables.
