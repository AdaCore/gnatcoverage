Core FCC requirements
====================

Core expectations for Functions and Call Coverage
(FCC) assessments. All the other FCC-related sections rely on this one.

For Function and Call Coverage assessments, the tool processes as subprograms
all functions and procedures and as calls all non-static call expressions and
statements.

In this context, the following set of rules shall be obeyed on top of the
requirements governing Statement Coverage:

======  ======================================================================
Rule #  Description
======  ======================================================================
1       When a call is never executed, the tool shall emit a call coverage
        violation for it.

2       When a call is executed at least once, it shall be reported as
        covered.

3       When a subprogram is never entered, it shall be reported as covered.

4       When a subprogram is entered at least once, no matter where from, it
        shall be reported as covered

5       Should the tool be unable to instrument a call or a subprogram, the
        coverage of the relevant call or subprogram shall be reported as
        undetermined.
======  ======================================================================

FCC has some limitations as gnatcov is unable to instrument the following
subprograms and calls:

- Instances of a generic subprogram
- Expression functions that are primitives of tagged types
- Calls made in the prefix of a dotted name

For these cases, source coverage obligations are emitted and the coverage
is reported as undetermined.

The tool is also unable to instrument and provide coverage information for
calls made to set the default value of a subprogram parameter or record
component. No source coverage obligations shall be emitted for these calls
and no coverage information reported.

.. rubric:: Testing Strategy


We validate all the FCC rules based on three main subsets of testcases:


.. qmlink:: SubsetIndexImporter

   *



