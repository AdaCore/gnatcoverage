.. _operational-conditions:

Operational conditions of use
=============================

This chapter describes rules on which the correctness of the source coverage
assessments performed by GNATcoverage depends.

.. _operational-context:

Operational Environment
-----------------------

The Operational Environment in which the tool is used shall match the
Qualification Environment in which the tool was qualified, as characterized by
the items presented in the following tables. The *Expected value* column here
states the intended value for qualification, which should match what
eventually gets logged within the |str_doc| report. The *Item #* column
provides item identifiers that are produced in this report to facilitate the
matching process.

For cross or native configurations:

.. tabularcolumns:: |p{0.06\textwidth}|p{0.30\textwidth}|p{0.60\textwidth}|

.. csv-table::
   :header: "Item #", "Description", "Expected value"
   :widths: 5, 30, 60
   :delim:  |

   e1 | Host Operating System name and version | Windows XP
   e2 | GNATcoverage executable name and version | gnatcov p.q.r
   e3 | GNAT Pro compiler executable name and version | powerpc-elf-gcc a.b.c (stamp)
   s1 | GNAT Pro compilation switches | -g -fpreserve-control-flow -fdump-scos -gnat05

For cross configurations only:

.. tabularcolumns:: |p{0.06\textwidth}|p{0.30\textwidth}|p{0.60\textwidth}|

.. csv-table::
   :header: "Item #", "Description", "Expected value"
   :widths: 5, 30, 60
   :delim:  |

   5 | GNATemulator executable name and version | powerpc-elf-gnatemu x.y.t

For the GNAT Pro compilation switches, additional options which do
not influence code generation, e.g. controlling warnings, are allowed.

Runtime Library Profile
-----------------------

The general contents of this document is not tailored for a particular runtime
library. You may for instance find the description of requirements and
testcases for features supported only by a Ravenscar or full native profile
even if the tool is used with a Zero-Footprint (ZFP) runtime.

The set of tests executed as part of a testsuite run is selected according
to the actual runtime used, nevertheless, based on the runtime name provided
through a :literal:`--RTS` compilation option, if any.

The important points regarding qualification are:

* If no :literal:`--RTS` option is passed for the qualification testuite
  run, users shall not pass a :literal:`--RTS` option either;

* If a :literal:`--RTS` option was passed for the qualification testsuite
  run, typically designating a runtime delivered with the GNAT Pro toolchain,
  users shall pass a :literal:`--RTS` option as well, designating a runtime
  with a ``system.ads`` identical to the qualification one.


Language Perimeter
------------------

As for the runtime libray profile, the general contents of this document is
not tailored for a particular version of the Ada language, except for the
:ref:`lrm-traceability` chapter. Otherwise, the requirements and testcases
sections might contain items matching up to Ada 2012 even if the intended
qualified use is for an Ada 95 or Ada 2005 project.

Nevertheless, to make sure there is no ambiguity on the intended use of the
tool for a given project:

* The version language for which qualification is sought shall be stated by
  way of a :literal:`-gnat95` :literal:`-gnat05` or :literal:`-gnat12`
  compilation option in the :ref:`operational-context` section.

In addition:

* The tool is not qualified to analyze programs featuring tasking constructs,
  controlled objects, or pointers to nested subprograms.

* The tool is only partially qualified for exceptions which propagate across
  subprograms, when the runtime library profile supports this at all. In such
  configurations, users shall verify that all uses of handlers for exceptions
  not raised by the subprogram itself conform to what our Operational Requirements
  prescribe (:ref:`exceptions`).

* For mcdc assessements, the tool requires the use of short-circuit variants
  for the Boolean binary operators composing decisions: ``&&`` or ``||`` in C,
  ``and then`` or ``or else`` in Ada.

* For decision or mcdc analysis, the tool is not qualified to assess coverage
  of expressions used in assertion constructs such as Assert pragmas or their
  contract programming model extensions in Ada 2012 (Pre/Post pragmas or
  aspects, their 'Class variants, static/dynamic subtype predicates or type
  invariants).

  This material is designed with the assumption that such constructs, if
  present in the source programs at all, are disabled, for instance thanks to
  an Assertion_Policy pragma.

* For statement, decision or mcdc analysis on Ada 2012, the tool is not
  qualified to assess coverage of the new forms of expression introduced in
  the language, in particular *conditional expressions*, *generalized
  membership tests* with more than one alternative, and *quantified
  expressions*.

  Such expressions are only allowed in assertion/contracts contexts, disabled
  for coverage analysis purposes as previously described in this section.
