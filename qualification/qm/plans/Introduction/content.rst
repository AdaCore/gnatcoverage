============
Introduction
============

This section contains an introduction to the qualification material for
GNATcoverage.


Document Purpose
================

The purpose of this document is to describe the applicable processes to qualify GNATcoverage; it also assigns specific activities to each interested party.

Definitions List
================

.. rubric:: GNATcoverage

A tool performing structural coverage analysis of Ada programs.

.. rubric:: Operational Environment

The environment in which GNATcoverage is used.

.. rubric:: Qualification Environment

The environment in which GNATcoverage is qualified.

.. rubric:: Test

A part of a testcase which exercises functional code in a precise manner, aimed at obtaining precise expected coverage results.

.. rubric:: Testcase

A part of the testing strategy used to verify a given Tool Operational Requirement.

.. rubric:: Tool Operational Requirement (TOR)

A TOR describes the expected behavior of a tool from the user point of view.


Referenced Documents
====================

.. rubric:: AE09

Bordin et al.: *Couverture: An Innovative Open Framework for Coverage Analysis
of Safety Critical Applications* - Ada User Journal, December 2009.

.. rubric:: DO-178B and ED-12B

RTCA DO-178B/EUROCAE ED-12B:
*Software Considerations in Airborne Systems and Equipment Certification*;
December 1992

.. rubric:: ERTS2010

Bordin et al: *Couverture: An Innovative and Open Coverage Analysis Framework
for Safety-Critical Applications* - ERTS2 2010

.. rubric:: GNAT Pro UG

AdaCore: *GNAT Pro User's Guide*, available as part of the GNAT Pro documentation
(file *share/doc/gnat/pdf/gnat_ugn.pdf*).

.. rubric:: GNATcoverage UG

AdaCore: *GNATcoverage User's Guide*, available with the GNATcoverage product
distribution (file *share/doc/gnatcoverage/pdf/gnatcov.pdf*).

.. rubric:: TOR document

AdaCore: *GNATcoverage Tool Operational Requirements*,
available with this qualification package and including testcases.

.. rubric:: PLANS document

AdaCore: *GNATcoverage Qualification Plans*,
this document.

.. rubric:: STR report

AdaCore: *GNATcoverage Software Test Results*,
available with this qualification package.


Organization And Roles
======================

Several parties are involved in the development, verification and
qualification process for GNATcoverage. The interested parties for GNATcoverage
(along with their responsibilities) are:

* **GNATcoverage Development Team**: Developers contribute to the development
  of GNATcoverage, including requirements specification, implementation, test
  cases development and test execution. This team is also in charge of the
  configuration management of the artifacts it produces.

* **GNATcoverage Qualification Team**: The Qualification Team is responsible
  for the infrastructure supporting the qualification process of GNATcoverage.
  The Qualification Team supports the Development Team. This team is also in
  charge of the configuration management of the artifacts it produces.

* **GNATcoverage Quality Assurance Team**: The Quality Assurance Team is a
  project-independent team responsible for ascertaining that the expected
  processes have been put in place. The Quality Assurance Team is granted
  the authority to require specific activities to be performed by the
  GNATcoverage Development and Qualification Teams. This team is also in charge
  of the configuration management of the artifacts it produces (mostly
  Quality Assurance reports).

* **GNATcoverage users**: GNATcoverage users are expected to perform the activities
  identified in section :qmref:`/PLANS/Tool_Qualification_Plan/User_Activities`
