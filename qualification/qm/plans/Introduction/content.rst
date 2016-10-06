============
Introduction
============

Document Purpose
================

This document is part of the GNATcoverage qualification material. It describes
the applicable processes to qualify GNATcoverage and assigns specific
activities to the responsible parties.

Referenced Documents
====================

.. rubric:: AE09

Bordin et al.: *Couverture: An Innovative Open Framework for Coverage Analysis
of Safety Critical Applications* - Ada User Journal, December 2009.

.. rubric:: DO-178B/ED-12B

RTCA DO-178B/EUROCAE ED-12B:
*Software Considerations in Airborne Systems and Equipment Certification*;
December 1992

.. rubric:: |standard|

RTCA DO-178B/EUROCAE ED-12C:
*Software Considerations in Airborne Systems and Equipment Certification*;
December 13, 2011

.. rubric:: DO-330/ED-215

RTCA DO-330/EUROCAE ED-215:
*Software Tool Qualification Considerations*;
December 13, 2011

.. rubric:: ERTS2010

Bordin et al: *Couverture: An Innovative and Open Coverage Analysis Framework
for Safety-Critical Applications* - ERTS2 2010

.. rubric:: GNAT Pro UG

AdaCore: *GNAT Pro User's Guide*, available as part of the GNAT Pro
documentation (file *share/doc/gnat/pdf/gnat_ugn.pdf*).

.. rubric:: GNATcoverage UG

AdaCore: *GNATcoverage User's Guide*, available with the GNATcoverage product
distribution (file *share/doc/gnatcoverage/pdf/gnatcov.pdf*).

.. rubric:: |plans_doc|

AdaCore: |project_name_it| - |plans_doc|, this document.

.. rubric:: |tor_doc|

AdaCore: |project_name_it| - |tor_doc|, included in this qualification package.

.. rubric:: |str_doc|

AdaCore: |project_name_it| - |str_doc|, included in this qualification package.

.. rubric:: |tqa_doc|

AdaCore: |project_name_it| - |tqa_doc|, accompanying this qualification package.

Definitions List
================

General Definitions
-------------------

The Annex B of [|tool_standard|] provides a list of acronyms and glossary of
common terms used throughout this qualification material.

Project Specific Definitions
----------------------------

.. rubric:: GNATcoverage

A tool performing structural coverage analysis of Ada programs.

.. rubric:: Operational Environment

The environment in which GNATcoverage is used.

.. rubric:: Qualification Environment

The environment in which GNATcoverage is qualified.

.. rubric:: Test

A part of a testcase which exercises functional code in a precise manner,
aimed at obtaining precise expected coverage results.

.. rubric:: Testcase

A part of the testing strategy used to verify a given Tool Operational
Requirement.

.. rubric:: Tool Operational Requirement (TOR)

A TOR describes the expected behavior of a tool from the user point of view.


Organization And Roles
======================

Here is the list of parties involved in the development, verification and
qualification process for GNATcoverage, along with their responsibilities:

* **GNATcoverage Development Team**: Developers contribute to the development
  of GNATcoverage, including requirements specification, implementation, test
  case development and test execution. This team is also in charge of the
  configuration management of the artifacts it produces.

* **GNATcoverage Qualification Team**: The Qualification Team is responsible
  for the infrastructure supporting the qualification process of GNATcoverage.
  The Qualification Team supports the Development Team. This team is also in
  charge of the configuration management of the artifacts it produces.

* **GNATcoverage Quality Assurance Team**: The Quality Assurance Team is
  independent of the Development Team and Qualification Team and is
  responsible for ascertaining that the expected processes have been put in
  place. The Quality Assurance Team is granted the authority to require
  specific activities to be performed by the GNATcoverage Development and
  Qualification Teams. This team is also in charge of the configuration
  management of the artifacts it produces (mostly Quality Assurance reports).

* **GNATcoverage users**: GNATcoverage users are expected to perform the activities
  identified in section :qmref:`/PLANS/Tool_Qualification_Plan/User_Activities`.
