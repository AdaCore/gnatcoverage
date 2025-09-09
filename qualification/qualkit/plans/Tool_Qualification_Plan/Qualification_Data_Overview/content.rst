.. _qualification-data:

Qualification Data Overview
===========================

GNATcoverage is qualified as a COTS tool according to guidance of
section 11.3 of [DO-330/ED-215]. This qualification kit documents the
activities required for the Tool Developer, and provides
recommendations for the Tool User to fulfill his part of the required
activities.

The qualification data that comes with the tool, composing the tool
*qualification kit*, consists of the following components:

* **PDF documents package**, containing the following three documents:

  * The |doc_title| document, which is this document
    and which we'll refer to as the |plans_abb| document.
 
  * The |tor_doc| document, which contains the description of
    the tool operational requirements together with the associated testing
    strategy and test cases, and which we'll refer to as the |tor_abb| document.

    The *Document Purpose and Organization* section of the |tor_abb| document
    includes a high level description of our overall testing strategy. The
    *Overview of the test procedures organization* appendix explains how
    expectations on coverage results for each test are stated and used to
    produce a verification report out of a testsuite run.

    The test sources aren't included in this version of the document. They are
    included in an HTML version, presented below.

  * The |str_doc| report, which contains a synthetic summary
    of the testsuite run performed to assess the tool behavioral correctness
    with respect to its intended use, and which we'll refer to as the |str_abb|
    report.

* **HTML documents package**, containing HTML versions of the PDF documents
  described above, presented differently and including test sources for the
  |tor_doc| item. The tests are organized in a very hierarchical manner
  so much easier to present and browse in this format than in PDF. See the
  :ref:`qa-activities` section of this document for a description of which
  |QA| activities are performed on which document variant.

* **Testsuite package**, archive of the testsuite directory where the
  qualification testsuite run took place, filtered to include artifacts
  of possible use for inspection (execution logs, coverage reports, ...),
  not binary artifacts such as object and executable files, as they take
  a huge amount of space, aren't so useful for inspection, and can easily
  be re-generated on demand if really needed.

* |tqa_doc| document, where all Quality Assurance cycles are tracked.
  
