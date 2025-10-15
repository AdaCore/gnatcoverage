SC requirements for disabled coverage regions
=============================================

For proper behavior of the Coverage Disabling facility, the
following set of rules shall be obeyed:

====== ======================================================================
Rule # Description
====== ======================================================================
1      Disabled regions shall all be synthesized in a distinct section of
       the output report, with a single message per region.

2      Each message describing a disabled region shall specify the range of
       source locations covered by the region declaration.

3      Disabled coverage regions shall not affect the diagnostics reported for
       constructs outside of such regions.

4      The report section dedicated to disabled coverage regions shall indicate
       the total number of disabled coverage regions and shall not contain any
       other kind of information.

5      Disabled coverage regions may be declared to disable coverage over
       various levels of syntactic constructs such as statements or subprograms,
       and groups of such in all the contexts where they may appear.

6      Disabled coverage annotations shall take precedence over exemption
       annotations.
====== ======================================================================


.. rubric:: Testing Strategy

The following set of testcases checks compliance with this requirement.
The testcases are partitioned into sections
according to point #5 - demonstrate ability to disable coverage for
various groups of syntactic constructs:


.. qmlink:: TCIndexImporter

   *



Each test contains a combination of disabled coverage regions and regular code,
exercised in several ways to cover well-identified sections of the program.
All the other aspects of the requirement (rules #1 to #4) are validated by
demonstrating that all the tests run as expected.
