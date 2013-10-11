=======================
Environment Equivalence
=======================

Qualification data is produced both in the qualification and in the user environments (see :qmref:`/PLANS/Tool_Qualification_Plan/User_Activities`). For the whole set of qualification material to be consistent, the qualification and user environments must therefore be equivalent. The equivalence of the following items is deemed sufficient to establish equivalence of environments:

#. The GNAT Pro executable name, version number and host operating system
#. The list of GNAT Pro compilation switches
#. The coding standard, if any, as enforced by pragma(s)
#. The run-time profile
#. The GNATemulator executable name, version number and host operating system
#. The GNATcoverage executable name, version number and host operating system

If all items above are the same in the two environments, then they are considered equivalent for the purpose of GNATcoverage usage.

The values corresponding to the environment where the qualification activities were performed are summarized in the "Operational Environment" section of the TOR document.
