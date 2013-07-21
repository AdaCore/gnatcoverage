SC expectations for If statements (ARM 5.3)
===========================================

SC expectations regarding If statements (ARM 5.3)


Requirement(s)
--------------



An IF statement comprises an IF *branch*, zero or more ELSIF branches and zero
or one ELSE branch. IF and ELSIF branches feature a control expression and all
the branches contain one or more statements. The nested statements get to
execute when the flow reaches the branch and the control expression, if any,
evaluates True.

In addition to the common requirements that apply to the nested statements,
IF and ELSIF branches that are never reached shall be reported uncovered.


Testing Strategy
----------------



We verify all the aspects of this requirement over

* Various forms of IF statements (with/without ELSIF branches, with/without
  ELSE branches),

* In various source contexts (regular functions or procedures, generic
  instances, package elaboration body),

All through a panel of branch selection schemes:


.. qmlink:: TCIndexImporter

   *


