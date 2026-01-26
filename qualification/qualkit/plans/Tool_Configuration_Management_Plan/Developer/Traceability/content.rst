.. _traceability:

Traceability
************

The following traceability relations are established (activity 7.2.2.f):

 * **From TOR to Test Cases:**

   TORs and Test Cases are organized in a hierarchical manner as a tree of
   artifacts. Each TOR and each Test Case is assigned a unique identifier which
   reflects the item's position in the tree.
    
   Each Test Case is associated with exactly one TOR. Test Cases are descendants
   of their associated TOR within the artifacts hierarchy, and the TOR
   identifier is then a common prefix of all the identifiers of Test Cases
   associated with it. 
 
   Operationally, TOR and Test Case artifacts are stored within file systems
   under configuration management control, with the
   hierarchy of artifacts mapped to file system folders.

 * **Other elements of traceability** are not required for TQL-5 tools.

All configuration items are traceable to the GNATcoverage (pre-)qualification
process (activity 7.2.2.g).

