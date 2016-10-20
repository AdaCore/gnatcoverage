Operational conditions of use
=============================

This chapter describes rules on which the correctness of the source coverage
assessments performed by GNATcoverage depends.

.. _operational-context:

Operational Environment
-----------------------

The Operational Environment in which the tool is used shall match the
Qualification Environment in which the tool was qualified, as characterized by
the items presented in the following tables. The *Expectation* column here
states the intended value for qualification, which should match what
eventually gets logged within the |str_doc| report.

For cross or native configurations:

.. tabularcolumns:: |p{0.06\textwidth}|p{0.30\textwidth}|p{0.60\textwidth}|

.. csv-table::
   :header: "Item #", "Description", "Expected value"
   :widths: 5, 30, 60
   :delim:  |

   1 | Host Operating System name and version | Windows XP
   2 | GNATcoverage executable name and version | gnatcov p.q.r
   3 | GNAT Pro compiler executable name and version | powerpc-elf-gcc a.b.c (stamp)
   4 | GNAT Pro compilation switches | -g -fpreserve-control-flow -fdump-scos -gnat05

For cross configurations only:

.. tabularcolumns:: |p{0.06\textwidth}|p{0.30\textwidth}|p{0.60\textwidth}|

.. csv-table::
   :header: "Item #", "Description", "Expected value"
   :widths: 5, 30, 60
   :delim:  |

   5 | GNATemulator executable name and version | powerpc-elf-gnatemu x.y.t

For the GNAT Pro compilation switches, additional options which do
not influence code generation, e.g. controlling warnings, are allowed.

Language revision and Runtime library profile
---------------------------------------------

The general contents of this document is not tailored for a particular runtime
library. You may for instance find the description of requirements and
testcases for features supported only by a Ravenscar or full native profile
even if the tool is used with a Zero-Footprint (ZFP) runtime.

The set of tests executed as part of a testsuite run is selected according
to the actual runtime used, nevertheless, based on the runtime name provided
through a :literal:`--RTS` compilation option, if any.

The important points regarding qualification are:

* If there is no :literal:`--RTS` option passed for the qualification testuite
  run, users shall not pass any :literal:`--RTS` option either;

* If there is a :literal:`--RTS` option passed for the qualification testsuite
  run, typically designating a runtime delivered with the GNAT Pro toolchain,
  users shall pass a :literal:`--RTS` option as well, designating a runtime
  with a ``system.ads`` identical to the qualification one.

Similarly, the general contents of this document is not tailored for a
particular version of the Ada language, so may include requirements and
testcases for up to Ada 2012 even if the intended qualified use is for an Ada
95 or Ada 2005 project.

Nevertheless, to make sure there is no ambiguity on the intended use of the
tool for a given project:

* The version language for which qualification is sought shall be stated by
  way of a :literal:`-gnat95` :literal:`-gnat05` or :literal:`-gnat12`
  compilation option in the :ref:`operational-context` section.

And the :ref:`lrm-traceability` chapter is tailored for this particular
version.

Coding standard requirements
----------------------------

* The tool is not qualified to analyze programs featuring tasking constructs,
  controlled objects, or pointers to nested subprograms.  Unless a
  ``system.ads`` corresponding to that of a Zero Footprint runtime is used,
  users shall ensure that the code to analyse compiles free of warnings out of
  the following configuration pragmas::

   pragma Restriction_Warnings (No_Tasking);
   pragma Restriction_Warnings (No_Implicit_Dynamic_Code);
   pragma Restriction_Warnings (No_Finalization);
   pragma Restriction_Warnings (No_Exception_Registration);
   
* The tool is only partially qualified for exceptions which propagate across
  subprograms. Unless a ``system.ads`` corresponding to that of a Zero
  Footprint runtime is used, users shall ensure that the code to analyse
  compiles free of warnings out of the following configuration pragma::

   pragma Restriction_Warnings (No_Exception_Handlers);

  or verify that all the handlers only feature simple assignment statements
  that never raise an exception.

* For mcdc assessements, the tool requires the use of short-circuit variants
  for the Boolean binary operators composing decisions: ``&&`` or ``||`` in C,
  ``and then`` or ``or else`` in Ada, as enforced by the
  ``No_Direct_Boolean_Operator`` Restrictions pragma.

* For decision or mcdc assessments, the tool is not qualified to evaluate
  expressions used in assertion constructs such as Assert pragmas or their
  contract programming model extensions in Ada 2012 (Pre/Post pragmas or
  aspects, their 'Class variants, static/dynamic subtype predicates or type
  invariants). This material is then designed with the assumption that such
  constructs, if present in the source programs at all, are disabled, for
  instance thanks to an Assertion_Policy pragma.

* For stmt, decision or mcdc assessments, the tool is also not qualified to
  evaluate *conditional expressions* (if-expressions and case-expressions)
  introduced by Ada 2012. From a source point of view, these are only allowed
  in assertion/contracts contexts, disabled for coverage analysis purposes as
  previously described in this section.
