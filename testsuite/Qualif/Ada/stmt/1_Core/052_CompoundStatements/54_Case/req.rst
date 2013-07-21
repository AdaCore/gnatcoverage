SC expectations for Case statements (ARM 5.4)
=============================================

SC expectations regarding Case statements (ARM 5.4)


Requirement(s)
--------------



A CASE statement starts with a *header* that introduces the controling
expression, followed by zero or more CASE alternatives and zero or one OTHERS
alternative. Each alternative contains other statements.

In addition to the common requirements that apply to the nested statements,
CASE statement headers that are never reached shall be reported uncovered.


Testing Strategy
----------------



We verify all the aspects of this requirement over

* A variety of CASE statement forms (with/without OTHERS alternative, with
  expression or range alternatives, with single/multi-value alternatives).

* In a variety of source contexts (regular functions or procedures, generic
  instances, package elaboration body),

All through a panel of alternative selection schemes:


.. qmlink:: TCIndexImporter

   *


