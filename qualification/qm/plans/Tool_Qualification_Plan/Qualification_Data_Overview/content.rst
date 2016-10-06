.. _qualification-data:

Qualification Data Overview
===========================

GNATcoverage is qualified as a COTS accoring to guidance of section 11.3 of
[DO-330/ED-215].

The qualification data that comes with the tool, composing the tool
*qualification kit*, consists in the two main packages:

* A **Documents package**, containing the following three documents:

  * The |plans_doc| document, |plans_pdf|, which is this document.

  * The |tor_doc| document, |tor_pdf|, which contains the description of the
    tool operational requirements together with the associated testing
    strategy and test cases.

    The *Document Purpose and Organization* section of this document includes
    a high level description of our overall testing strategy. The *Overview of
    the test procedures organization* appendix of this document explains how
    expectations on coverage results for each test are stated and used to to
    produce a verification report out of a testsuite run.

    The test sources aren't included in this document. They are included in an
    HTML version of the document, part of the Testsuite package presented
    below.

  * The |str_doc| report, |str_pdf|, which contains a synthetic summary of the
    testsuite run performed to assess the tool behavioral correctness with
    respect to its intended use.

* A **Testsuite package**, composed of:

  * An HTML version of the TOR document, including the test sources, organized
    in a very hierarchical manner so much easier to present and browse in this
    format than in PDF.

  * An archive of the testsuite directory where the qualification testsuite
    run took place, including all the execution artifacts (actual reports
    produced by the tool for each test, intermediate object files, execution
    traces produced by the instrumented execution environment, ...).

