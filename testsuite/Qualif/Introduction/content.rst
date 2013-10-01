Document purpose and organization
*********************************

Document Purpose
================

This document is part of the GNATcoverage qualification material, allowing the
use of a well identified version of the tool to perform structural coverage
assessments in accordance with a qualified interface.

The document describes the environment within which the tool is expected to
operate, then the qualified interface as a set of *Tool Operational
Requirements* validated by *Testcases*.

The qualification focuses on the ``--annotate=report`` text output of
GNATcoverage, which provides a list of violations with respect to a given
coverage criterion, such as ``statement not executed at <file>:<line>:<col>``.

Organization Guidelines
=======================

Beyond this introduction and the operational environment description, most of
the document describes the Tool Operational Requirements and the associated
Testcases along the following guidelines:

A **Tool Operational Requirement** (or **TOR**) describes the proper behavior
of the GNATcoverage tool for a specific language construct with regards to a
given coverage criterion. For example, how shall the tool behave on Ada `if`
statements when performing Decision Coverage assessments.

To help expose a structured organization, related requirements are bundled
together as named **Requirement Groups**. For example, the Statement Coverage
requirements for Ada `if`, `case`, `block` and `loop` statements are bundled
together as the `CompoundStatements` group.

The tool conformance to each requirement is validated by a set of
**Testcases**, organized according to a **Testing Strategy** dedicated
to this requirement and described together with it.

Many requirements call for multiple testcases. As for requirements, sets of
related testcases which share common grounds are constructed and referred to
as **Testcase Groups**.

For each language and coverage criterion of relevance, we distinguish among
different categories of requirements based on expectations regarding:

* *Core coverage metrics*, for example to validate statement coverage
  assessments on conditional constructs, loops, etc.  Testcases for such
  TORs typically exercise a piece of functional code in various ways, for
  example by causing a Boolean expression to be evaluated only True or False,
  and verify that results are as expected in all the variations.  Programming
  language reference manuals are a major source for identifying
  the relevant constructs.

* *General coverage analysis facilities*, for example the support for
  coverage exemptions or consolidation capabilities.
  In addition to validating the tool behavior with respect to the stated
  requirements, testcases in this category extend the set of exercised code
  samples where mutliple language features are used in combination.

Orthogonally, the *output report format* is considered part of the tool
qualified interface as well so is subject to a dedicated set of requirements
and testcases. Some of these requirements are also implicitly validated by the
execution of all the coverage checking testcases in other categories, where
specific sections of the report are scanned by the testsuite harness to search
for criteria violation messages. 

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

