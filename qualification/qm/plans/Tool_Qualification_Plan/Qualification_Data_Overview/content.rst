.. _qualification-data:

Qualification Data Overview
===========================

GNATcoverage is qualified as a COTS accoring to guidance of section 11.3 of
[DO-330/ED-215].

The qualification data that comes with the tool, composing the tool
*qualification kit*, consists in the two main packages:

* A **PDF documents package**, containing the following three documents:

  * The |plans_doc| document, |plans_pdf|, which is this document.

  * The |tor_doc| document, |tor_pdf|, which contains the description of the
    tool operational requirements together with the associated testing
    strategy and test cases.

    The *Document Purpose and Organization* section of the TORs document
    includes a high level description of our overall testing strategy. The
    *Overview of the test procedures organization* appendix explains how
    expectations on coverage results for each test are stated and used to to
    produce a verification report out of a testsuite run.

    The test sources aren't included in this version of the document. They are
    included in an HTML version, presented below.

  * The |str_doc| report, |str_pdf|, which contains a synthetic summary of the
    testsuite run performed to assess the tool behavioral correctness with
    respect to its intended use.

* A **HTML documents package**, containing HTML versions of the PDF documents
  described above, presented differently and including test sources for the
  |tor_doc| item. The tests are organized in a very hierarchical manner so
  much easier to present and browse in this format than in PDF. See the
  :ref:`qa-activities` section of this document for a description of which
  |QA| activities are performed on which document variant.

* A **Testsuite package**, archive of the testsuite directory where the
  qualification testsuite run took place, including all the execution
  artifacts (actual reports produced by the tool for each test, intermediate
  object files, execution traces produced by the instrumented execution
  environment, ...).

