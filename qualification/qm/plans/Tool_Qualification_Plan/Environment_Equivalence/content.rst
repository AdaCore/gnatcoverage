=======================
Environment Equivalence
=======================

Qualified use of the tool requires equivalence of the Operational Environment
and the Qualification Environment, based on the following values:

#. The GNAT Pro compiler executable name, version number and host operating system
#. The list of GNAT Pro compilation switches
#. The GNAT Pro runtime profile
#. The application coding standard, if any, as enforced by pragma(s)
#. The GNATemulator executable name, version number and host operating system
#. The GNATcoverage executable name, version number and host operating system

If all items above are the same in the two environments, then they are
considered equivalent for the purpose of GNATcoverage usage.

The values corresponding to the environment for which the qualification
activities were intended are summarized in the *Operational Environment*
section of the TOR document, and the values corresponding to the environment
where the qualification testsuite was run are summarized in the *Qualification
Environment* section of the STR document.
