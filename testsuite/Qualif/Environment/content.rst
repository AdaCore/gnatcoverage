Operational Environment
=======================

The correctness of the source coverage assessments performed by GNATcoverage
relies on a few rules that shall be obeyed regarding the environment in which
the tool operates.

The table below lists the expectations assessing the Operational
Environment equivalence described in the tool qualification *PLANS* document.

.. tabularcolumns:: |p{0.05\textwidth}|p{0.30\textwidth}|p{0.60\textwidth}|

.. csv-table::
   :header: "Rule #", "Item", "Expectation"
   :widths: 5, 30, 60
   :delim:  |

   1 | GNAT Pro compiler executable name and version | powerpc-elf-gcc a.b.c (stamp)
   2 | GNAT Pro compilation switches | -g -fpreserve-control-flow -fdump-scos -gnat05
   3 | GNAT Pro runtime profile | Zero Footprint (RTS=zfp or an equivalent system profile wise, such as the zfp-prep or zfp-p2020 variants tailored for specific boards)
   4 | Application coding standard | For a level A software level, the binary boolean operators used to compose decisions shall be restricted those with short-circuit semantics, as enforced by the ``No_Direct_Boolean_Operator`` Restrictions pragma for Ada.
   5 | GNATemulator executable name and version | powerpc-elf-gnatemu x.y.t
   6 | GNATcoverage executable name and version | gnatcov p.q.r
   7 | Host Operating System name and version | Windows XP

Additional compilation options are allowed which do not influence code
generation, such as for controlling warnings.

The tool behavior correctness shall be verified by a complete testsuite
run configured for the target qualification level, producing a *Software Test
Results* report clear of any test failure.
