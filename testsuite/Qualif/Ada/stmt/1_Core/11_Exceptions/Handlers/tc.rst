Check SC correct recognition of alternate exception handlers for user
=====================================================================

Check SC correct recognition of alternate exception handlers for user
defined or language standard exceptions

Exercise conditional explicit raise of user defined exception or implicit
raise of predefined exception, covered by distinct or common handlers. Check
that

* Exception handlers are all reported as uncovered when no exception is
  raised,

* Only the statements associated to the correct handler are reported as
  covered when an exception is raised,

Check exception handlers with explicit exception names and including the
"others" choice.
