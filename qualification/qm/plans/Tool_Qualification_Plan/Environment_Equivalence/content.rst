=======================
Environment Equivalence
=======================

Qualification data are produced both in the qualification and user environments (see see :qmref:`$(parent)::User_Activities`). For the whole set of qualification material to be consistent, the qualification and user environments must thus be equivalent. We deem the following data sufficient to establish equivalence of environments:

#. the GNAT Pro executable name, version number and host operating system;
#. the GNAT Pro compilation switches;
#. the GNATcoverage executable name, version number and host operating system.

If all points above are equal, then the two environments are equivalent. It is important to highlight that GNATcoverage qualification material is **always** tailored for a **precise** context **only**: such context is identified by all points above plus the coding standard itself.
