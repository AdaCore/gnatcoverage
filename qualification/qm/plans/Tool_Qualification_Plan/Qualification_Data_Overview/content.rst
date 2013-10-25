.. _qualification-data:

Qualification Data Overview
===========================

The qualification data that comes with the tool, composing what we refer to as
a *qualification kit*, consists in the following three documents:

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
our overall testing strategy. The :ref:`adacore-cm` section of this (PLANS)
document provides an overview of the tree structure.

Testcase subdirectories hold all the necessary items allowing automated
execution of the tests; execution scripts in addition to the test sources
typically. The artifacts filesystem used to construct the TOR document is thus
also used to drive entire *testsuite* runs and produce the STR report for a
given software level. The "Overview Of The Testsuite Structure" appendix of
the TOR document explains how expectations on coverage results are stated for
this purpose.
