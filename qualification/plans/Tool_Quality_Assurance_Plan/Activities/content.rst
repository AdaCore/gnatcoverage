.. _qa-activities:

|QA| Activities
===============

This section provides the overview of objectives of distinct |QA| activities
applied to the documents composing a qualification kit. Output of |QA|
activities is recorded in the separate sections of the |tqa_doc| document.

The PDF format offers a sequential layout of the material which allows
browsing from end to end in a reliable manner, so most of the QA operations
described here are performed on the PDF version of the documents. Only those
steps requiring access to test sources are performed with the help of the HTML
contents.

Reading the |plans_doc| document
********************************

**Objectives:**

  * Assess the compliance with qualification objectives.

Inspection of Tool Operational Requirements
*******************************************

**Objectives:**

  * Check the accuracy, completeness and consistency with respect to the
    qualification objectives.

The inspection is conducted by sampling, according to the following
guidelines:

The requirements are split into families corresponding to the major kind of
metrics to be assessed. For DAL C, there is only a "Statement Coverage"
family; for DAL B, there is an additional "Decision Coverage" family and for
DAL A, a "Modified Condition/Decision Coverage" family complements the other
two. Each metric family is further divided into groups and possibly subgroups,
as needed.

This is all summarized in the *Document Purpose and Organization* chapter of
the |tor_doc| document, section *Organization Guidelines*, which the |QA|
reviewer reads first.

The reviewer then selects a set of requirements for review and this set is
documented in the |QA| report. The set would include at least stmt/Exemptions,
Common/Report and some other requirement(s) at the discretion of the reviewer.

Inspection of Test Cases
************************

**Objectives:**

  * Check the accuracy of test cases, in particular whether the tests exercise
    their target Ada constructs.

This activity is also performed by sampling by the |QA| reviewer, selecting
all or a subset of the test cases associated with the selected requirements,
also documented in the |QA| report.

To help understanding the tests performed for a test case, the reviewer first
gets acquainted with the *Overview of the test Procedures Organization*
appendix of the |tor_doc| document, where we describe the specific syntax we
use to state expected coverage results.

Inspection of test execution results
************************************

**Objectives:**

  * Check that the Verification Environment reported in the |str_doc| report
    is equivalent to the Operational Environment described in the |tor_doc|
    document.

  * Check the results of test execution.

  * In the case tests failed, it is necessary to investigate whether the
    source of error is:

    * A misbehavior of the infrastructure used to run tests and compare actual
      results to expected results: in this case, the |project_name|
      Qualification Team is in charge of reporting and fixing the problem.

    * A bug in the |project_name| implementation: in this case, the
      |project_name| Development Team is in charge of reporting and fixing the
      problem.

    * A reasonable limitation of the tool: in this case, the |project_name|
      Qualification Team is in charge of reporting and justifying the problem
      as part of the known limitations of the tool.

