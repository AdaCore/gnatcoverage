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

Reading the PLANS document
**************************

**objectives:** 

  * Assess the compliance with qualification objectives.

Inspection of Tool Operational Requirements (by sampling)
*********************************************************

**objectives:** 

  * Check the accuracy, completeness and consistency with respect to the
    qualification objectives.


Inspection of Test Cases (by sampling)
**************************************

**objectives:** 

  * Check the accuracy of test cases, in particular whether the tests exercise
    their target Ada constructs.


Inspection of test execution results
************************************

**objectives:** 

  * Check that the Qualification Environment reported in the |str_doc| report
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

