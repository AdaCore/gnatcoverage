Core expectations for Statement Coverage (SC) assessments.
==========================================================

Core expectations for Statement Coverage (SC) assessments.
All the other SC related sections rely on this one.

To ensure coverage of all the relevant language constructs, we decompose the
material further in accordance with the Ada Reference Manual (ARM):

%(subset-index)s

A few chapters are not included for the following reasons:

.. csv-table::
   :header: "Chapter", "Not included because ..."
   :widths: 30, 60
   :delim:  |

   ARM chap. 1 : General                | No language construct described
   ARM chap. 4 : Names and Expressions | "The described constructs are not
   considered on their own for coverage analysis purposes. The coverage
   information is computed for enclosing statement or declaration constructs."
   ARM chap. 9 : Tasks and Synchronization | "The execution profile being
   qualified is based on Zero Foot Print run-time, which does not support any
   construct described in this chapter"
   ARM chap. 13 : Representation Issues | "Constructs described in this
   chapter do not result in executable code so they are of no interest for
   coverage analysis."

