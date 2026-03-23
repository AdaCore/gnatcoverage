SC requirements for Pragmas (ARM 2.8)
=====================================




Different kinds of pragmas exist, some possibly generating code such
as `Debug` or `Assert` pragmas. From the compiler standpoint, these
are either active or ignored depending on `Assertion_Policy`,
`Check_Policy` controls or alike. For statement coverage analysis,
they shall be considered as statements on their own according to the
following rules:

- With binary traces, a pragma is considered as a statement in
  accordance with the actual Assertion/Debug/Check policy applicable
  to the unit where the pragma appears, which makes it active or
  inactive from the compiler's standpoint.

- With source instrumentation, Assert-like pragmas (Assert,
  Assert_And_Cut, Check, Precondition/Postcondition, Assume,
  Loop_Invariant, Type_Invariant) are never considered as statements
  on their own and all the other pragmas are always considered as
  statements, regardless of the Assertion/Debug/Check policy applying to
  their unit.

.. rubric:: Testing Strategy

Exercise Assert, Debug, Precondition and Postcondition pragmas placed
in various source contexts, explicitly either activated or deactivated
by local Check_Policy or Debug_Policy.

Check that pragmas considered as statements but never elaborated are
reported uncovered and that other pragmas are ignored.

.. qmlink:: TCIndexImporter

   *

