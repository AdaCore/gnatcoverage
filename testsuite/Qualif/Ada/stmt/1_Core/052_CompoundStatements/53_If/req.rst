SC expectations for If statements (ARM 5.3)
===========================================


Requirement(s)
--------------



An IF statement comprises an IF branch, zero or more ELSIF branches and zero
or one ELSE branch. IF and ELSIF branches contain a control expression, and all
the branches contain one or more statements. The nested statements
execute when control reaches the branch and the control expression, if any,
evaluates to True.

In addition to the common requirements that apply to the nested statements,
IF and ELSIF branches that are never reached shall be reported uncovered.


Testing Strategy
----------------



We verify compliance with this requirement through tests exercising
various forms of IF statements (with/without ELSIF branches, with/without
ELSE branches), in a variety of source contexts (subprograms, generic
instances, package elaboration bodies).

The following table summarizes the testcases:


.. qmlink:: TCIndexImporter

   *


