Core SC requirements
====================

All the other SC-related sections rely on this group.

To ensure coverage of all the relevant language constructs, we decompose the
material further based on the organization of the Ada Reference Manual (ARM):


.. qmlink:: SubsetIndexTable

   *


Several chapters are not included for the following reasons:

.. tabularcolumns:: |p{0.3\textwidth}|p{0.65\textwidth}|

.. csv-table::
   :header: "Chapter", "Not included because ..."
   :widths: 28, 65
   :delim:  |

   ARM chap. 1 : General                | No language construct described
   ARM chap. 4 : Names and Expressions | "The described constructs are not
   considered on their own for coverage analysis purposes. The coverage
   information is computed for enclosing statement or declaration constructs."
   ARM chap. 9 : Tasks and Synchronization | "The execution profile being
   qualified is based on Zero Foot Print run-time, which does not support any
   construct described in this chapter"
   ARM chap. 13 : Representation Issues | "Constructs described in this
   chapter do not result in executable code and thus are not relevant to
   coverage analysis."



.. qmlink:: SubsetIndexTocTree

   *

