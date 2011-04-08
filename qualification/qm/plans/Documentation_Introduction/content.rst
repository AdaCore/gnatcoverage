==========================
Documentation Introduction
==========================

This section contains an introduction to qualification material for GNATcoverage.


Document Purpose
================

The purpose of this document is to describe the applicable processes to qualify GNATcoverage; it also assigns specific activities to each interested party.

Definitions List
================

.. rubric:: Environment

The context on which GNAT check is used.

.. rubric:: GNATcoverage

A tool performing structural coverage analysis of Ada programs.

.. rubric:: Qualification environment

The environment in which GNATcoverage is qualified.

.. rubric:: Report

A file containing the GNATcoverage output.

.. rubric:: Test

A test is an concretization of a testcase with precise values and parameters.

.. rubric:: Testcase

A part of the testing strategy used to verify a given Tool Operational Requirement.

.. rubric:: Tool Operational Requirement (TOR)

A TOR describes the expected behaviour of a tool from the point of view of the user.

.. rubric:: User environment

The environment in which GNATcoverage is used.

Referenced Documents
====================

.. rubric:: AE09

Bordin et al.: *Couverture: An Innovative Open Framework for Coverage Analysis
of Safety Critical Applications* - Ada User Journal, December 2009.

.. rubric:: DO-178B and ED-12B

EUROCAE : SOFTWARE CONSIDERATIONS IN AIRBORNE SYSTEMS AND EQUIPMENT CERTIFICATION

.. rubric:: ERTS2010

Bordin et al: *Couverture: An Innovative and Open Coverage Analysis Framework
for Safety-Critical Applications* - ERTS2 2010

.. rubric:: GNAT Pro UG

AdaCore: GNAT Pro User Guide. Available as part of GNAT Pro documentation.

.. rubric:: GNATcoverage RM

AdaCore: GNATcoverage Reference Manual.

Organization And Roles
======================

Several parties are involved in the development, verification and qualification process for GNATcoverage. The interested parties for GNATcoverage (along with their responsibilities) are:

* **GNATcoverage Development Team**: developers contribute to the development of GNATcoverage , including requirements specification, implementation, test cases development and test execution. This team is also in charge of the configuration management of the artifacts it produces.

* **GNATcoverage Qualification Team**: the Qualification Team is responsible for the infrastructure supporting the qualification process of GNATcoverage . The Qualification Team supports the development team. This team is also in charge of the configuration management of the artifacts it produces.

* **GNATcoverage Quality Assurance Team**: the Quality Assurance Team is a project-independent team responsible to ascertain the expected processes have been put in place. The Quality Assurance Team is granted the authority to require specific activities to be performed by the GNATcoverage Development and Qualification Teams. This team is also in charge of the configuration management of the artifacts it produces (mostly Quality Assurance reports).

* **GNATcoverage users**: GNATcoverage users are expected to perform the activities identified in section :qmref:`$(project)::Plans::Tool Qualification Plan::User_Activities`.
