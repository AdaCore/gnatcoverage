.. _operational-conditions:

Operational Conditions of Use
=============================

This chapter describes rules on which the correctness of the source coverage
assessments performed by GNATcoverage depends. This instance of the document
is valid for use in a certification context no stricter than DAL |prj_DAL|.

.. _operational-context:

Operational Environment
-----------------------

The Operational Environment in which the tool is used shall match the
Verification Environment in which the tool was qualified, as characterized by
the items presented in the following table. The *Expected value* column here
states the intended value for qualification, which should match what
eventually gets logged within the |str_doc| report. The *Item #* column
provides item identifiers that are produced in this report to facilitate the
matching process.

.. tabularcolumns:: |p{0.06\textwidth}|p{0.30\textwidth}|p{0.60\textwidth}|

.. csv-table:: Expected verification Environment values
   :header: "Item #", "Description", "Expected value"
   :widths: 5, 30, 60
   :delim:  /

   e1 / Host Operating System family / |prj_host_platform|
   e2 / GNATcoverage version / |gnatcov_version|
   e3 / GNAT Pro platform version / |gnatpro_version|
   s1 / GNAT Pro compilation switches / |prj_switches|
   s2 / GNAT Pro Runtime Library Profile / |prj_RTS|


For the GNAT Pro compilation switches, additional options which do not
influence code generation are allowed. This may, for example, include options
controlling warnings, confirming compiler defaults, or designating a
configuration pragma file which contains only pragmas not influencing code
generation (``Restriction_Warnings`` pragmas or specific ``Restrictions`` such
as ``No_Direct_Boolean_Operators`` for instance).

The Runtime Library Profile indication conveys the :literal:`--RTS` switch for
which the Verification Environment should be setup, designating a runtime
library delivered with the GNAT Pro toolchain. Tool Users shall have their
toolchain setup to compile programs with a :literal:`--RTS` switch as well,
designating a runtime with a ``system.ads`` identical to the qualification one.

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
|plans_doc| document.


.. _language-scope:

Language Version and Scope
--------------------------

The general contents of this document is not tailored for a particular
version of the Ada language. The requirements and testcases sections
might contain items matching up to Ada 2012 even if the intended
qualified use is for an Ada 95 or Ada 2005 project.

Nevertheless, to make sure there is no ambiguity on the intended use of the
tool for a given project:

* The version language for which qualification is sought shall be stated by
  way of a :literal:`-gnat95` :literal:`-gnat05` or :literal:`-gnat12`
  compilation option in the :ref:`operational-context` section.

In addition:

* The tool is not qualified to analyze programs featuring tasking constructs,
  controlled type definitions or pointers to nested subprograms.

  One possible way to ensure that the code subject to analysis conforms to
  what this qualification material encompasses is to verify that it compiles
  without error under control of a Zero Footprint Runtime Library Profile, or
  free of warnings out of the following set of configuration pragmas::

    pragma Restriction_Warnings (No_Tasking);
    pragma Restriction_Warnings (No_Finalization);
    pragma Restriction_Warnings (No_Implicit_Dynamic_Code);

* For mcdc assessments, the tool requires the use of short-circuit variants
  for the Boolean binary operators composing decisions: ``&&`` or ``||`` in C,
  ``and then`` or ``or else`` in Ada.

  One possible way to ensure this for Ada is to verify that the code subject
  to analysis compiles without error under control of the following
  configuration pragma::

     pragma Restrictions (No_Direct_Boolean_Operator);

* For decision or mcdc analysis, the tool is not qualified to assess coverage
  of expressions used in assertion constructs such as Assert pragmas or their
  contract programming model extensions in Ada 2012 (Pre/Post pragmas or
  aspects, their 'Class variants, static/dynamic subtype predicates or type
  invariants).

  This material is designed with the assumption that such constructs, if
  present in the source programs at all, are disabled, for instance thanks to
  an Assertion_Policy pragma.

* For statement, decision or mcdc analysis on Ada 2012, the tool is not
  qualified to assess coverage of *generalized membership tests* with more
  than one alternative.

  Such expressions are only allowed in assertion/contracts contexts, disabled
  for coverage analysis purposes as previously described in this section.
