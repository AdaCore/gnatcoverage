.. _adacore-conf-methods:

Configuration Management Methods and Activities
***********************************************

Base technical support
----------------------

Configuration Management of artifacts is technically implemented via Git
repositories which track the life cycle of each artifact automatically.
Discussions occurring during the lifecycle are tracked using the GitLab instance
deployed at AdaCore.

.. _adacore-change-control:

Change control
--------------

Integrity of configuration items is guaranteed by Git repositories where all
configuration items are located. Only authorized engineers can modify
configuration items and all modifications are recorded. The sources and scripts
for the AdaCore products are maintained in Git repositories that are completely
backed up. All repositories are stored electronically
and are backed up to geographically isolated locations in AWS (in EU and US).
This increases AdaCore's confidence in the durability of qualification data
(activity 7.2.4.a).

Each change to a configuration item is associated to a unique change identifier,
which is used to track the version of a configuration item over its history
(activity 7.2.4.b).

.. _adacore-archive:

Archive, Retrieval and Release
------------------------------

Repositories are available for audit if necessary (activity 7.2.7.a).

Only authorized engineers can change the configuration
items, thanks to the security mechanisms embedded in the Git
repositories (activity 7.2.7.b.1).

All repositories used in tool executable object code production and
qualification, and all binaries delivered to AdaCore clients are stored
forever (activity 7.2.7.e).

Detailed procedures are as follows.

Customer/Project specific tracking
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

For each specific customer/project qualification, an internal *kit
identifier* is assigned. It is referenced for example in QA reports. All the corresponding
verification related artifacts are managed on a dedicated *branch* within
AdaCore's version control systems.

Releases of qualification kits for the
specific customer/project are produced off that branch, which also tracks the
QA cycles performed on the kit items (QA reports, corresponding corrections,
etc).

A typical early change incorporated on the branch is the adjustment
of the targeted operational environment parameters, which will be accounted
for when setting up the Qualification Environment for kit production cycles.


Official baseline production
^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Official baselines are generated for customer-specific deliveries aimed at a
precise operational environment and software level. Once the configuration
management branch dedicated to such deliveries is set up, a complete testsuite
run is performed in the corresponding Qualification Environment.

A kit version number is then assigned and the qualification data documents
are produced.

The resulting set of documents is packaged as zip files comprising the kit.
This kit then goes through QA as needed, and all or part of
this cycle repeats until a positive agreement on the kit "acceptability" for
release is reached.

