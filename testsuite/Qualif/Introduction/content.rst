Introduction
============

Document Purpose
----------------

This document is part of the GNATcoverage qualification material.
It consists of the "Tool Operational Requirements" and the associated 
"Test Cases".  

A "Tool Operational Requirement" describes the proper behavior of the
GNATcoverage tool for a specific language feature.  Several related
requirements compose a "requirement group".  Each requirement describes
its Testing Strategy, using a specially chosen test case (or set of
test cases, referred to as a "test case group") to demonstrate correct
coverage analysis.  We also specify requirements/tests to verify the
expected format of the tool's output.

Abbreviations
-------------
The following abbreviations are used throughout this document.

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

