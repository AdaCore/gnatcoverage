FCC requirements for calls to subprograms in nested packages
============================================================

FCC should not be influenced by the fact that subprograms can be nested in
one or more packages. This is always true expect in the case of a call to a
functions returning a type which is not visible at the call site, in which
case the coverage of this call is expected to be undetermined.

.. rubric:: Testing Strategy

This requirement is validated with the following test cases:

.. qmlink:: TCIndexImporter

  *



