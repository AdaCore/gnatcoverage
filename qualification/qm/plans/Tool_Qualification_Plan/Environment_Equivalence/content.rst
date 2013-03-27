=======================
Environment Equivalence
=======================

Qualification data is produced both in the environment where qualification activities are performed and in the environment where the qualified tool is used (see :qmref:`../User_Activities`). For the whole set of qualification material to be consistent, those 2  environments must be equivalent. The equivalence of the following items is deemed sufficient to establish equivalence of environments:

#. The GNAT Pro executable name, version number and host operating system;
#. The list of GNAT Pro compilation switches;
#. The GNATemulator executable name, version number and host operating system;
#. The GNATcoverage executable name, version number and host operating system.

If all items above are the same in the 2 environments, then they are considered equivalent for the purpose of GNATcoverage usage.

The values corresponding to the environment where the qualification activities were performed are summarized in the "Execution Context Summary" section of the STR reports. 

The "Operational Environment" section of the TOR document states general rules regarding the set of supported GNAT Pro versions and compilation options.


 