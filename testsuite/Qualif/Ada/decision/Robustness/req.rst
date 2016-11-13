DC requirements for potentially confusing constructs
====================================================

%REQ_ID%

Combined Statement and Decision Coverage assessments shall remain correct in
the presence of code constructs that require sophisticated analysis by the
coverage tool not to end up confused by the visible or internal complexity,
e.g on decisions spanning multiple lines or on use of controlled types
exposed by the runtime library when the profile allows it.

.. rubric:: Testing Strategy

Check a variety of cases where simple analysis schemes could
cause inaccuracies or errors in coverage diagnostics:


.. qmlink:: TCIndexImporter

   *


