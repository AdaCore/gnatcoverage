Document purpose and organization
*********************************

Document Purpose
================

This document is part of the GNATcoverage qualification material, allowing the
use of a well identified version of the tool to perform structural coverage
assessments in accordance with a qualified interface.  The document describes
the environment within which the tool is expected to operate, then a set of
*Tool Operational Requirements* validated by *Testcases*.

The qualification focuses on the ``--annotate=report`` text output of
GNATcoverage, which provides a list of statement, decision, or mcdc violations
of relevance to the requested analysis.

Organization Guidelines
=======================

Beyond this introduction and the Operational Environment description, most of
the document describes the Tool Operational Requirements and the associated
Testcases along the following guidelines:

A **Tool Operational Requirement** (abbreviated as **TOR** or **Requirement**)
describes the proper behavior of the GNATcoverage tool for a specific language
construct with regards to a given coverage criterion. For example, how shall
the tool behave on Ada `if` statements when performing Decision Coverage
assessments.
To organize the material in a structured manner, related requirements are
bundled together as named **Requirement Groups**. For example, the Statement
Coverage requirements for Ada `if`, `case`, `block` and `loop` statements are
bundled together as the `CompoundStatements` group.

The tool conformance to each requirement is validated by a set of
**Testcases**, organized according to a **Testing Strategy** dedicated to this
requirement and described together with it. Many requirements call for
multiple testcases. As with requirements, sets of related testcases which
share common grounds are composed and referred to as **Testcase Groups**.

Throughout this document, tables introducing local sets of qualification
artifacts are inserted to list items of interest at the point where the table
appears. For example a table listing all the testcases and groups attached to
a given requirement is always included within the testing strategy description
for this requirement. Each line in such a table designates a particular
testcase, requirement or group artifact, with an abbreviation denoting the
kind of artifact in the first column, the artifact local name in the second
column and a short description of the artifact contents in the third.  See the
:ref:`abbrevs` section later in this chapter for the list of abbrevations used
in the first column.

At the top of the hierarchy, collections of high level requirement groups are
collected in **Chapters** and **Subchapters**, typically per language and
coverage criterion of relevance. We will for example have an `Ada` chapter
containing a `stmt` subchapter to collect the Ada Statement Coverage toplevel
requirement groups.

For language related chapters, we distinguish two main categories of
requirement groups, addressing:

* *Core coverage metrics*, for example to validate statement coverage
  assessments on conditional constructs, loops, etc.  Testcases for such TORs
  typically exercise a piece of functional code in various ways, for example
  by causing a Boolean expression to be evaluated only True or False, and
  verify that results are as expected in all the variations.  Programming
  language Reference Manuals, such as the Ada 2005 LRM for Ada, are the major
  source for identifying the relevant constructs.

* *General coverage analysis facilities*, for example the support for coverage
  exemptions or consolidation capabilities.  In addition to validating the
  tool behavior with respect to the stated requirements, testcases in this
  category are also typically designed to exercise multiple kinds of language
  constructs mixed together. They represent a good opportunity to do so as
  they are not directed to focus on specific contructs by the nature of the
  requirements to exercise, unlike a core requirment on, say, *If* statements
  for example.

We also have :ref:`language-independent-tors` chapter to hold language
independent requirements and testcases, essentially regarding the *output
report format* which is considered part of the tool qualified interface. Some
of these requirements are also implicitly validated by the execution of all
the coverage checking testcases in other chapters, where specific sections of
the report are scanned by the testsuite harness to search for criteria
violation messages.

Downtree, each Testcase is assigned a unique **Testcase Identifier**, computed
from its local name (chosen to be representative of its purpose) and position
in the chapter/groups hierarchy. This identifier is denoted as a path with
slashes between each intermediate level, for example
`/TOR/Ada/stmt/Core/CompoudStatements/Loop/Incomplete_Iteration`. These
identifiers are used to denote individual testcases in the Software Test
Results (STR) report produced out of testuite runs for qualification.

For a more detailed description of how testscase sources are organized and how
expected results are stated, please refer to the :ref:`testsuite-overview`
section of this material.

.. _abbrevs:

Abbreviations
=============

The following abbreviations are used throughout this document:

.. csv-table::
   :delim: |
   :widths: 30, 40
   :header: "Abbreviation", "Meaning"

   DC|Decision Coverage
   MCDC|Modified Condition/Decision Coverage
   SC|Statement Coverage
   STMT|Statement
   TOR|Tool Operational Requirement
   rq|requirement
   rqg|requirement group
   tc|test case
   tcg|test case group

