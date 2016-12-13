.. _operational-conditions:

|opcond_section_title|
======================

This chapter describes rules on which the correctness of the source coverage
assessments performed by GNATcoverage depends.

.. _operational-context:

Operational Environment
-----------------------

The Operational Environment in which the tool is used shall match the
Qualification Environment in which the tool was qualified, as characterized by
the items presented in the following table. The *Expected value* column here
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
   s2 | GNAT Pro Runtime Library Profile | --RTS=<qualification-rts-name>

For cross configurations only:

.. tabularcolumns:: |p{0.06\textwidth}|p{0.30\textwidth}|p{0.60\textwidth}|

.. csv-table::
   :header: "Item #", "Description", "Expected value"
   :widths: 5, 30, 60
   :delim:  |

   5 | GNATemulator executable name and version | powerpc-elf-gnatemu x.y.t

For the GNAT Pro compilation switches, additional options which do not
influence code generation, e.g. controlling warnings or confirming compiler
defaults, are allowed.

The Runtime Library Profile indication conveys the --RTS switch for which the
Qualification Environment should be setup, designating a runtime library
delivered with the GNAT Pro toolchain. Tool Users shall have their toolchain
setup to compile programs with a :literal:`--RTS` switch as well, designating
a runtime with a ``system.ads`` identical to the qualification one.

Installation, Documentation and Qualified Interface
---------------------------------------------------

All AdaCore tool packages use a common installation procedure, that depends on
the host platform.

* On Windows hosts, the tool packages are provided as
  self-installing executables. The Tool User should run the executable
  corresponding to the tool he wants to install, then follow the instructions
  on screen.
  
* On non-Windows hosts, the tool packages are provided as .tar.gz
  archives. The Tool User should unpack the archive corresponding to the tool
  he wants to install, then run the script doinstall located at the root of
  the archive directory structure, and follow the instructions on screen.

General use of the tool is described in the Tool User's Guide document, which
can be found in the subdirectory :literal:`share/doc` of the tool
installation.  For qualified use more specifically, the Tool User shall also
conform to the rules described in the *Qualified Interface* section of the
qualification |plans_doc| document.


Language Version and Scope
--------------------------

As for the runtime library profile, the general contents of this document is
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
  controlled type definitions or pointers to nested subprograms.

* The tool is only partially qualified for analysis in presence of exceptions
  which propagate across subprograms, when the runtime library profile
  supports this at all.

  For all uses, in subprograms subject to the analysis, of handlers for
  exceptions not raised by the subprogram itself, users shall verify
  conformance to what the Operational Requirements specifically prescribe
  for such cases (:ref:`exceptions`).

* For mcdc assessments, the tool requires the use of short-circuit variants
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
