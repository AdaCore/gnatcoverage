=======================
Environment Equivalence
=======================

Qualified use of the tool requires equivalence of the Operational Environment
and the Qualification Environment, based on the following criteria:

#. The GNAT Pro compiler executable name and version number
#. The list of GNAT Pro compilation switches
#. The GNAT Pro runtime profile
#. The application coding standard, if any, as enforced by pragma(s)
#. The GNATemulator executable name and version number
#. The GNATcoverage executable name and version number
#. The host Operating System

The two environments are considered equivalent for the purpose of GNATcoverage
usage if all the items above are the same in both of them.

The values corresponding to the environment for which the qualification
activities were intended are described in the *Operational Environment*
section of the TOR document. The values corresponding to the environment where
the qualification testsuite was run are summarized in the *Qualification
Environment* section of the STR report.

