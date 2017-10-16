.. _adacore-conf-methods:

Configuration Management Methods and Activities
***********************************************

This section describes the internal configuration management process at
AdaCore. It is not related to the configuration management process deployed by
the applicant.

Base technical support
----------------------

Configuration Management of artifacts is technically implemented via Git
repositories which track the life cycle of each artifact automatically.
E-mail-based discussions about each artifact are also tracked, using the
AdaCore ticket system deployed within the whole company since 1998.

.. _adacore-change-control:

Change control
--------------

Integrity of configuration items is guaranteed by the Git
repositories where all configuration items are located. Only authorized
engineers can modify configuration items and all modifications are
recorded. In addition, all repositories and mail servers are mirrored with
machines physically located in Paris (France) and New York. This increases
confidence in the durability of qualification data (activity 7.2.4.a).

Each change to a configuration item is associated to a unique
ID, which unambiguously identifies the version of a configuration item over
its history (activity 7.2.4.b).

.. _adacore-archive:

Archive, Retrieval and Release
------------------------------

Repositories are available for audit if necessary (activity 7.2.7.a).

Only authorized engineers can change the configuration
items, thanks to the security mechanisms embedded in the Git
repositories (activity 7.2.7.b.1).


Detailed procedures are as follows.

Customer/Project specific tracking
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

For each specific customer/project qualification we assign an internal *kit
identifier*, referenced for example in QA reports. All the corresponding
verification related artifacts are managed on a dedicated *branch* within
AdaCore's version control systems. Releases of qualification kits for the
specific customer/project are produced off that branch, which also tracks the
QA cycles performed on the kit items (QA reports, corresponding corrections,
...). A typical kind of early change incorporated on the branch is the
adjustment of the targeted operational environment parameters, to be accounted
for when setting up the verification environment for kit production cycles.


Official baseline production
^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Official baselines are generated for customer-specific deliveries aimed at a
precise operational environment and software level. Once the configuration
management branch dedicated to such deliveries is set up, a complete testsuite
run is performed in the corresponding verification environment. A kit version
number is then assigned and the qualification data documents are produced.
The resulting set of documents is packaged as a zip file which materializes
the kit as a whole. This kit then goes through QA as needed and all or part of
this cycle repeats until a positive agreement on the kit "acceptability" for
release is reached.

