.. _qualification-data:

Qualification Data Overview
===========================

The qualification data that comes with the tool, which we will refer to
collectively as the tool's "qualification kit", consists of the following
three PDF documents:

* The **PLANS** document, *PLANS.pdf*, which is this document.
* The **TOR** (*Tool Operational Requirements*) document, *TOR.pdf*, which
  contains the description of the tool operational requirements and
  the associated testcases.
* The **STR** (*Software Test Results*) report, *STR.pdf*, which contains a
  synthetic summary of the testsuite run performed to assess the tool
  behavioral correctness with respect to its intended use.

The TOR document, core of the qualification data set, is elaborated from a
hierarchically organized set of *artifacts* (requirements, testcases, test
sources, ...) stored within a filesystem tree. The *Document Purpose and
Organization* section of the TOR document includes a high level description of
our overall testing strategy.

Testcase subdirectories hold all the necessary items allowing automated
execution of the tests; execution scripts in addition to the test sources in
particular. The artifacts filesystem used to construct the TOR document is
thus also used to drive entire *testsuite* runs and produce the STR report for
a given software level. The *Overview of the test procedures organization*
appendix of the TOR document explains how expectations on coverage results are
stated for this purpose. An archive of the testsuite directory where the
qualification run takes place is provided together with the corresponding
kit delivery.

The test sources aren't included in the PDF version of the TOR document, out
of size considerations.  They are included in an HTML version of the document,
also provided together with the PDF delivery and where they are easily
accessible thanks to the HTML navigation capabilities.  Except for the test
sources, the HTML version mirrors the PDF contents, just presented
differently.
