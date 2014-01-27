SC requirements for exemption regions
=====================================

%REQ_ID%

For proper behavior of the Coverage Exemptions facility, the
following set of rules shall be obeyed:

====== ======================================================================
Rule # Description
====== ======================================================================
1      Exempted regions shall all be synthesized in a distinct section of
       the output report, with a single message per region.

2      Each exemption message shall specify the range of source locations
       covered by the region declaration, and indicate whether 0 or more
       coverage violations were actually exempted within this range.

3      Exemption regions shall not affect the diagnostics reported for
       constructs outside of such regions.

4      The report section dedicated to exemption regions shall not contain
       any other kind of information.

5      Exemption regions may be declared to exempt various levels of
       syntactic constructs such as statements or subprograms, and groups of
       such in all the contexts where they may appear.
====== ======================================================================


.. rubric:: Testing Strategy

The following set of testcases checks compliance with this requirement.
The testcases are partitioned into sections
according to point #5 - demonstrate ability to exempt
various groups of syntactic constructs:


.. qmlink:: TCIndexImporter

   *



Each test contains a combination of exempted regions and regular code,
exercised in several ways to cover well-identified sections of the program.
All the other aspects of the requirement (rules #1 to #4) are validated by
demonstrating that all the tests run as expected.

For each stated expectation, exempted region, or non-exempted
violation, the testsuite driver checks if it appears in the expected report
section (in addition to its regular checks).

