.. index::
   single: Coverage Checkpoints

.. _checkpoints:

*********************************************************************
Consolidation from :term:`coverage checkpoints <Coverage Checkpoint>`
*********************************************************************

A :term:`coverage checkpoint <Coverage Checkpoint>` is a file containing a
condensed version of a coverage assessment computed by |gcvcov|, which the tool
can reload and combine with other assessments very efficiently. This may be
viewed as an internal representation of a coverage report which
:cmd-option:`--annotate` would output in a user readable form.

As reports, checkpoints are initially produced from traces, thanks to a
:cmd-option:`--save-checkpoint` option to |gcvcov|.  The switch expects the
name of the checkpoint file to create as an argument, as in::

  gnatcov coverage --level=<> <units> <traces> --save-checkpoint=<filename>

:cmd-option:`--checkpoint` options then allow consolidating sets of checkpoints
together to produce a user level report and/or another checkpoint. The example
command below shows how to reuse the previous checkpoint to produce a user
readable report::

  gnatcov coverage --level=<> --checkpoint=<filename> --annotate=<>


The second :cmd-option:`--level` option may differ from the first, as long as
it does not request a stricter criterion. Assessing ``stmt`` or
``stmt+decision`` coverage from checkpoints containing ``stmt+mcdc`` data is
allowed, not the other way around.

In user readable reports, the set of traces eventually contributing to the
coverage result is available from the index page for HTML formats and in the
``Assessment Context`` header section of :cmd-option:`=report` outputs. In the
latter case, if the contribution comes from an intermediate checkpoint, the
command which produced the checkpoint is displayed. For example, a sequence of
commands such as::

  obj/pgm1  # producing trace1
  obj/pgm2  # producing trace2

  gnatcov coverage --level=<> trace1 <units> --save-checkpoint=t1.ckpt
  gnatcov coverage --level=<> trace2 <units> --checkpoint=t1.ckpt --annotate=report


Would produce a report with this kind of information in the header::

   Trace files:

   trace2
   kind     : source
   program  : obj/pgm2
   date     : 2020-09-22 16:32:52
   tag      :

   trace1
   kind     : source
   program  : obj/pgm1
   date     : 2020-09-22 16:32:47
   tag      :
   processed: gnatcov coverage <...> --save-checkpoint=t1.ckpt @ 2020-09-22 16:35:06.61

An important point of note: the specification of units of interest
applies to *traces* only, never to input checkpoints, regardless of
the checkpoints use.

The ``<units>`` in the second command of our previous example serve
to scope the coverage achieved by the ``trace2`` argument and has no
influence, for the output report, on the units held by ``t1.ckpt``.

Suppose we had used different sets of units for the first and second
commands, say::

    gnatcov coverage --level=<> trace1 <a b c> --save-checkpoint=t1.ckpt
    gnatcov coverage --level=<> trace2 <b c> --checkpoint=t1.ckpt --annotate=report

The output report produced by the second command would convey the
coverage achieved for unit "a" out of ``trace1`` (as held by
``t1.ckpt``), together with the coverage for units "b" and "c" out of
both ``trace1`` (as held by t1.ckpt) and ``trace2`` (direct
input). The possible coverage for unit "a" by ``trace2`` is filtered
out (not in the set of units of interest), but the coverage for that
unit held by the checkpoint input is included in the report.

We will now describe a few example situations of possible checkpoint uses, then
discuss compatibility issues regarding consolidation.

Efficient storage of test results
=================================

Trace files can be large and often need to be kept together with the
executable files that were used to produce the traces, for traceability
reasons for example, or for specific processing operations in some
circumstances. Preserving collections of traces and executables to allow
consolidation can be problematic, space-wise.

In some situations (depending on the overall size of programs, number of units
of interest, assessed coverage criteria, number and structure of decisions,
...) a checkpoint obtained from a trace can be a lot smaller than the trace +
executable size combination.

When that is the case for most executions in a testing campaign, a possibility
to improve storage efficiency consists in producing a checkpoint for each trace
and preserve just that to consolidate afterwards, as in::

  gnatcov coverage --level=<> <units> <trace1> --save-checkpoint=ckpt1
  (possibly, remove <trace1> as well as the corresponding executable)
  ...
  gnatcov coverage --level=<> <units> <traceN> --save-checkpoint=ckptN


Then consolidation can be achieved with::

  gnatcov coverage --level=<> --annotate=<> --checkpoint=ckpt1 ... --checkpoint=ckptN


or, using a response file with the list of checkpoint names::

  gnatcov coverage --level=<> --annotate=<> --checkpoint=@<ckpts.list>



As checkpoints contain high level coverage information, they are a lot more
efficient to aggregate, which is all the more beneficial if that processing
is performed repeatedly.

For situations where individual checkpoints are more often larger than
their respective trace and executable, a checkpoint can still be of
interest to store consolidated results for a group of tests, after all
the traces are produced::

  gnatcov coverage --level=<> <units> @<traces.list> --save-checkpoint=<filename>


Another possibility consists in accumulating results in a common
checkpoint as each test gets processed. This is referred to as
:ref:`incremental_analysis`, described in the following section:

.. _incremental_analysis:

Incremental coverage analysis
=============================

:term:`Incremental Coverage Analysis` consists in constructing a consolidated
coverage result incrementally, using a single checkpoint file to accumulate new
info about discharged coverage obligations as tests execute in sequence.

When providing execution traces as *the only* input to |gcvcov|, coverage
analysis starts with an empty coverage state where none of the coverage
obligations in units of interest are discharged; that is, the units are
all considered entirely not covered.
Providing a checkpoint on input to |gcvcov| simply instructs the tool to start
from the previously computed coverage state stored in the given checkpoint.
When traces are also provided, the tool consolidates the coverage achieved by
the traces with that of the initial state and the result can be output as a
report and/or saved in a checkpoint again.

Reusing an input checkpoint file as the output is allowed, and the computation
of a cumulative coverage result by an entire testsuite is then possible with a
sequence of commands such as::

  # Process test1.trace, saving the resulting coverage state in a newly
  # created checkpoint:
  gnatcov coverage --level=<> -P<project> test1.srctrace \
                   --save-checkpoint=testsuite.ckpt

  # Process subsequent test traces test2.srctrace .. testN.srctrace, each time
  # starting with the coverage state reached at the previous iteration,
  # and saving the resulting coverage state in the same checkpoint file
  # (overwriting it):
  gnatcov coverage --level=<> -P<project> test2.srctrace \
                   --checkpoint=testsuite.ckpt --save-checkpoint=testsuite.ckpt
  ...
  gnatcov coverage --level=<> -P<project> testN.srctrace \
                   --checkpoint=testsuite.ckpt --save-checkpoint=testsuite.ckpt

  # Now produce a report from the cumulated results:
  gnatcov coverage --level=<> --checkpoint=testsuite.ckpt --annotate=<>


The big advantage of this approach is that it stores everything in a single
checkpoint, which will occupy much less space than when using a checkpoint per
trace and might then also be more efficient than preserving traces and
executables.

This is very useful for situations where a given set of units is exercised by
such a large testsuite that it is impractical to preserve the executables and
execution traces, or individual checkpoints for all tests.

The individual coverage results are lost, however, and the (heavier) trace
processing steps cannot be done in parallel since they all read and write a
common checkpoint file which |gcv| does not lock.

.. _incidental_coverage:

Handle incidental coverage effects
==================================

In this scenario, we are assuming that two units A and B are being tested,
that contain calls to each other.  Each unit has its own testsuite, based on
its specific requirements: testsuite A covers the requirements for unit A, and
testsuite B covers the requirements for unit B. Running the two testsuites
produces two sets of trace files, one set for testsuite A and one set for
testsuite B.

Now suppose that you want to assess the global coverage for a system comprising
both unit A and unit B. If the two sets of trace files are consolidated
using a single execution of |gcvcov| as in::

  gnatcov coverage --level=stmt --units=A --units=B --annotate=report \
                   testA1.srctrace ... testAN.srctrace \
                   testB1.srctrace ... testBN.srctrace

then calls to B made by A while running testsuite A will contribute
to discharging coverage obligations for unit B, and the other way round.
This "incidental coverage" may be undesirable, as testsuite A is meant
to exercise the requirements of unit A only (not unit B) and so should
not contribute to the coverage of unit B.

This can be resolved using checkpointed coverage state, because each separate
run of |gcvcov| can consider a different set of units of interest -- traces
processed in each run will only contribute to the coverage of the units of
interest for that run.

A consolidated coverage report can thus be constructed using a two pass
analysis::

  # Discharge the coverage obligations for unit A (--units=A) using
  # trace files from testsuite A.
  gnatcov coverage --level=stmt --units=A \
                   testA1.srctrace ... testAN.srctrace \
                   --save-checkpoint=testsuiteA.ckpt

  # Discharge the coverage obligations for unit B (--units=B) using
  # trace files from testsuite B, consolidate with previous results from
  # testsuite A (--checkpoint), and produce a report (--annotate).
  gnatcov coverage --level=stmt --units=B \
                   testB1.srctrace ... testBN.srctrace --checkpoint=testsuiteA.ckpt \
                   --annotate=report

In a consolidated report produced following this procedure, each set of trace
files contributes only to the coverage of the units of interest specified for
the execution of |gcvcov| in which it is processed.


Compatibility considerations
============================

Allowed coverage criteria combinations
--------------------------------------

A bit like source traces which hold results for some pre stated strictest
possible criterion (at instrumentation time), checkpoints hold results
computed for a given coverage criterion, provided to the |gcvcov| command used
to produce the checkpoint. Computing results (e.g. a report) from such
checkpoints may not request a criterion stricter than the least strict of the
criteria held by the checkpoints. For instance, from a set of checkpoints
established for *stmt+decision* and *stmt+mcdc*, one may request the
production of a report for at most *stmt+decision*. Requesting *stmt* alone
would be fine as well, and *stmt+mcdc* would be rejected because one of the
checkpoints doesn't contain precise enough information.

Checkpoint format versions
--------------------------

The format of checkpoint files sometimes needs to evolve to support new
functionality and each format is identified by a version number stored within
the checkpoints. Maintaining degraded modes for old formats in a given version
of |gcp| proved very intricate and error prone so a |gcp| designed for
checkpoint format *N* now just rejects attempts at processing checkpoints of a
different version. Not every new release of |gcp| incurs a change of format
though, and we hope that incompatibilities will only rarely turn out
annoying in practice.
