.. _traceability:

Traceability
************

The following traceability relations are established (activity 7.2.2.f):

 * **From TOR to Testcases:**

   TORs and Testcases are organized in a hierarchical manner as a tree of
   artifacts. Each TOR and each Testcase is assigned a unique identifier which
   reflects the item's position in the tree.
    
   Each Testcase is associated with exactly one TOR. Testcases are descendants
   of their associated TOR within the artifacts hierarchy, and the TOR
   identifier is then a common prefix of all the identifiers of Testcases
   associated with it. 
 
   Operationally, TOR and Testcase artifacts are stored within filesystems
   under Git or Subversion configuration management control, with the
   hierarchy of artifacts mapped to filsystem folders.

 * **Other elements of traceability** are not required for TQL-5 tools.

All configuration items are traceable to the GNATcoverage (pre-)qualification
process (activity 7.2.2.g).

