.. index::
   single: Coverage State Checkpoints

.. _checkpoints:

**************************
Coverage State Checkpoints
**************************

At the end of a source coverage analysis performed using |gcvcov|, a
certain coverage state is reached, which comprises the identification of a
set of units of interest, and the indication of what coverage obligations
(for some coverage criterion) have been discharged using the provided
execution traces. The output of |gcvcov|, either as a set of annotated
sources, or as a report of coverage violations (i.e. of coverage
obligations that have not been discharged) reflects this coverage state.

When providing execution traces as the only input to |gcvcov|, coverage
analysis starts with an empty coverage state where none of the coverage
obligations in units of interest are discharged: they are entirely not
covered.  However, in some situations it is desirable to use the result of
a previous analysis, rather than an empty state, as the starting point
for a subsequent run. This is achieved by saving the first coverage
state as a checkpoint, and then reloading it in a subsequent run.

Use Case 1: Incremental Coverage Analysis
=========================================

In this scenario, we are assuming that a given set of units is
exercised using such a large testsuite that it is impractical
to have the execution traces for all tests simultaneously available
for processing by a single consolidating run of |gcvcov|.

In this case, cumulative coverage achieved by the complete test
suite can be computed using checkpoints to carry over intermediate
coverage results::

  gnatcov coverage --level=stmt --scos=@alis --trace=test1.trace \
                   --save-checkpoint=testsuite.ckpt
  # Process test1.trace, saving resulting coverage state in a newly created
  # checkpoint testsuite.ckpt

  gnatcov coverage --level=stmt --scos=@alis --trace=test2.trace \
                   --checkpoint=testsuite.ckpt --save-checkpoint=testsuite.ckpt
  ...
  gnatcov coverage --level=stmt --scos=@alis --trace=testN.trace \
                   --checkpoint=testsuite.ckpt --save-checkpoint=testsuite.ckpt
  # Process subsequent test traces test2.trace .. testN.trace, each time
  # starting with the coverage state reached at the previous iteration,
  # and saving the resulting coverage state in the same checkpoint file
  # (overwriting it). The report file output at each iteration represents
  # the cumulative achieved coverage so far.

Use Case 2: Incidental Coverage Avoidance
=========================================

In this scenario, we are assuming that two units A and B are being tested,
that contain calls to each other.  Each unit has its own testsuite,
based on its specific requirements: testsuite A covers the requirements
for unit A, and testsuite B covers the requirements for unit B.

Running the two testsuites produces two sets of trace files:
`testA1.traces` ... `testAN.trace` for testsuite A, `testB1.traces`
... `testBN.trace` for unit B.

Now suppose that you want to assess the global coverage for a system comprising
both unit A and unit B. If these two sets of trace files are consolidated
using a single execution of |gcvcov|::

  gnatcov coverage --level=stmt --scos=a.ali --scos=b.ali --annotate=report \
                   --trace=testA1.traces ... --trace=testAN.trace \
                   --trace=testB1.traces ... --trace=testBN.trace

then calls to B made by A while running testsuite A will contribute
to discharging coverage obligations for unit B, and the other way round.
This "incidental coverage" may be undesirable, as testsuite A is meant
to exercise the requirements of unit A only (not unit B) and so should
not contribute to the coverage of unit B.

This can be resolved using checkpointed coverage state, because each separate run
of |gcvcov| can consider a different set of units of interest -- traces
processed in each run will only contribute to the coverage of the units of
interest for that run.

A consolidated coverage report can thus be constructed using a two pass analysis::

  gnatcov coverage --level=stmt --scos=a.ali \
                   --trace=testA1.traces ... --trace=testAN.trace \
                   --save-checkpoint=testsuiteA.ckpt
  # Discharge the coverage obligations for unit A (unit of interest) using
  # trace files from testsuite A.

  gnatcov coverage --level=stmt --scos=b.ali --annotate=report \
                   --trace=testB1.traces ... --trace=testBN.trace \
                   --checkpoint=testsuiteA.ckpt
  # Discharge the coverage obligations for unit B (unit of interest) using
  # trace files from testsuite B, and consolidate with previous results
  # from testsuite A.

In a consolidated report produced following this procedure, each set of trace files contributes
only to the coverage of the units of interest specified for the execution of |gcvcov| in which
it is processed, and the information of which run each trace was processed in is included in the
report.

