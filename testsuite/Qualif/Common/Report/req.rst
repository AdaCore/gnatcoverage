Describe the tool output report format and check compliance to it
==================================================================


Requirement(s)
--------------



The tool qualified output is the synthetic report produced by the
--annotate=report command line option. The output report shall:

* Start with an explicit "COVERAGE REPORT" indication

* End with an explicit "END OF REPORT" indication

* Feature up to four sections in between: "Assessment Context", "Coverage
  violations", "Exempted Regions" (optionally), and "Analysis summary"
 
* The "Assessment Context" section shall summarize all the elements
  of relevance to characterize the reported assessment unambiguously:

  * Tool version identifier

  * Date and time of execution

  * Full command line issued to produce the report

  * Coverage criteria assessed (e.g. "stmt" or "stmt+decision")

  * Information about each of the input trace files:

    * trace tag & file name
    * trace production date & time
    * program executable file name

* The "Coverage violations" section shall feature a set of subsections,
  one for each criterion assessed with

  * A list of all the criterion obligations unsatisfied (violated) by
    the set of executions conveyed by the provided execution traces, and

  * A count of those violations

* The "Exempted Regions", when present, shall

  * Summarize information about all the exemption regions in the scope of the
    examined coverage obligations, and

  * End with a count of those regions

  It shall only be present when exemption regions were declared in the sources
  of interest.

* The "Analysis summary" section shall expose

  * A single synthetic line for each coverage criterion assessed, with a count
    of the non-exempted violations of that particular criterion.
  
  * A single synthetic line providing the count of exempted regions.


Testing Strategy
----------------



We provide testcases to check that the reports produced by the tool satisfy
the requirements in a range of different situations:


.. qmlink:: TCIndexImporter

   *



In each of these testcases, we validate the presence and structure of all the
expected sections and items in the "Assessment Context" part. We don't verify
the general correctness of reported violations vs expectations here. This is
defined and validated by sibling chapters of this TOR document, in particular
those dedicated to specific coverage criteria.

The output report correctness also gets validated as part of those other
chapters, with the testsuite engine checking that:

* All the reported coverage violations are found in the correct report
  section,

* The synthetic counters at the end of sections reporting violations or
  exempted regions is correct (matches the actual number of items reported in
  the section),

* The synthetic counters reported in the global "Analysis Summary" section are
  the same as those reported in individual sections,

* There is an exact correspondance between the reported items and the testcase
  expectations, with a test passing only if everything reported was stated as
  expected and everything expected was found to be reported.

