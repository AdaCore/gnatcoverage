SC requirements for exemption regions
=====================================

For proper behavior of the Coverage Exemptions facility, the
following set of rules shall be obeyed:

====== ======================================================================
Rule # Description
====== ======================================================================
1      Exempted regions shall all be synthesized in a distinct section of
       the output report, with a single message per region.

2      Each message describing an exempted region shall specify the range of
       source locations covered by the region declaration, and indicate whether
       0 or more coverage violations were actually exempted within this range.

3      The type and location of exempted violations shall be displayed under
       the corresponding exempted region message.

4      Exemption regions shall not affect the diagnostics reported for
       constructs outside of such regions.

5      The report section dedicated to exemption regions shall indicate the
       total number of exemption regions and total number of exempted
       violations, and shall not contain any other kind of information.

6      Exemption regions may be declared to exempt various levels of
       syntactic constructs such as statements or subprograms, and groups of
       such in all the contexts where they may appear.
====== ======================================================================


.. rubric:: Testing Strategy

The following set of testcases checks compliance with this requirement.
The testcases are partitioned into sections
according to point #6 - demonstrate ability to exempt
various groups of syntactic constructs:


.. qmlink:: TCIndexImporter

   *



Each test contains a combination of exempted regions and regular code,
exercised in several ways to cover well-identified sections of the program.
All the other aspects of the requirement (rules #1 to #5) are validated by
demonstrating that all the tests run as expected.

For each stated expectation, exempted region, exempted violation and 
non-exempted violation, the testsuite driver checks if it appears in the
expected report section (in addition to its regular checks).

