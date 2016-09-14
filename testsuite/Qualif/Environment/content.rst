The correctness of the source coverage assessments performed by GNATcoverage
relies on rules regarding

* The operational environment in which the tool operates, 

* The runtime library profile used by the assessed programs,

* Some possible extra constraints to be enforced on the application coding
  standard.

Operational Environment
=======================

The table below lists the expectations assessing the Operational
Environment equivalence described in the tool qualification *PLANS* document.

.. tabularcolumns:: |p{0.05\textwidth}|p{0.30\textwidth}|p{0.60\textwidth}|

.. csv-table::
   :header: "Rule #", "Item", "Expectation"
   :widths: 5, 30, 60
   :delim:  |

   1 | GNAT Pro compiler executable name and version | powerpc-elf-gcc a.b.c (stamp)
   2 | GNAT Pro compilation switches | -g -fpreserve-control-flow -fdump-scos -gnat05
   3 | GNATemulator executable name and version | powerpc-elf-gnatemu x.y.t
   4 | GNATcoverage executable name and version | gnatcov p.q.r
   5 | Host Operating System name and version | Windows XP

Additional compilation options are allowed which do not influence code
generation, such as for controlling warnings.

Runtime library profile
=======================

This material assumes the use of a Zero Footprint kind of runtime profile, as
would be enforced by the use of a --RTS=zfp switch or an equivalent system
profile wise, such as the zfp-prep or zfp-p2020 variants tailored for specific
boards.

Additional coding standard requirements
=======================================

For mc/dc assessements, the tool requires the use of short-circuit variants
for the Boolean binary operators composing decisions: `&&` or `||` in C, `and
then` or `or else` in Ada, as enforced by the ``No_Direct_Boolean_Operator``
Restrictions pragma.

For decision or mc/dc assessments, the tool is not qualified to evaluate
expressions used in assertion constructs such as Assert pragmas or their
contract programming model extensions in Ada 2012 (Pre/Post pragmas or
aspects, their 'Class variants, static/dynamic subtype predicates or type
invariants). This material is then designed with the assumption that such
constructs, if present in the source programs at all, are disabled, for
instance thanks to an Assertion_Policy pragma.

For stmt, decision or mc/dc assessments, the tool is also not qualified to
evaluate *conditional expressions* (if-expressions and case-expressions)
introduced by Ada2012. From a source point of view, these are only allowed in
assertion/contracts contexts, disabled for coverage analysis purposes as
previously described in this section.
