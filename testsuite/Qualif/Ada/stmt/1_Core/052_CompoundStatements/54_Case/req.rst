SC requirements for Case statements (ARM 5.4)
=============================================


.. rubric:: Requirement(s)



A CASE statement starts with a *header* that introduces the controlling
expression, followed by zero or more CASE (**when**) alternatives
and zero or one OTHERS
alternative. Each alternative contains one or more statements.

In addition to the common requirements that apply to the nested statements,
CASE statement headers that are never reached shall be reported uncovered.


.. rubric:: Testing Strategy



We verify all the aspects of this requirement over

* A variety of CASE statement forms (with/without OTHERS alternative, with
  expression or range alternatives, with single/multi-value alternatives).

* In a variety of source contexts (regular functions or procedures, generic
  instances, package elaboration body),

All through a collection of alternative selection schemes:


.. qmlink:: TCIndexImporter

   *


