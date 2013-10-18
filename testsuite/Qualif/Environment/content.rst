Operational Environment
=======================

The correctness of the source coverage assessments performed by GNATcoverage
relies on a few rules that shall be obeyed regarding the environment in which
the tool operates.

The table below describes the expectations assessing the Operational
Environment equivalence described in the tool qualification *PLANS* document.

.. tabularcolumns:: |p{0.05\textwidth}|p{0.30\textwidth}|p{0.60\textwidth}|

.. csv-table::
   :header: "Rule #", "Item", "Expectation"
   :widths: 5, 30, 60
   :delim:  |

   1 | GNAT Pro compiler executable name, version & host OS | powerpc-elf-gcc 7.0.3 (20130909-45) on Windows XP
   2 | GNAT Pro compilation switches | -g -gnat05 -fpreserve-control-flow -fdump-scos
   3 | GNAT Pro runtime profile | Zero Footprint (RTS=zfp or equivalent)
   4 | Application coding standard | For a level A software level, the binary boolean operators used to compose decisions shall be restricted those with short-circuit semantics, as enforced by the ``No_Direct_Boolean_Operator`` Restrictions pragma for Ada.
   5 | GNATemulator executable name, version and host OS | powerpc-elf-gnatemu 1.3.1 on Windows XP
   6 | GNATcoverage executable name, version and host OS | gnatcov 1.2.1 on Windows XP


