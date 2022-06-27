.. _consolidation:

**********************
Coverage Consolidation
**********************

:dfn:`Coverage consolidation` designates the general facility allowing the
computation of the overall coverage achieved by a set of executions. When
different executions get through common :term:`units of interest`,
consolidation computes the combined coverage of all the executions on these
units.

A typical case where consolidation is useful is when some part of an
application depends on external inputs and several executions are required to
exercise different scenarios in the application program. The execution traces
to consolidate are obtained from the same executable in this case.
Another common situation is when execution of different executables is needed
to achieve the required coverage for a software, either because distinct
software modules are tested independently (e.g. the different units of a
library), or because different aspects of the behavior of modules are
tested separately (e.g. the different subprograms of a library unit or
different scenarios of a given subprogram).

The production of consolidated coverage reports can proceed either directly
from a set of source or binary traces produced by the executions, or from a
set of pre-computed partial results latched in what we refer to as
:term:`coverage checkpoints <Coverage Checkpoint>`, which offer a lot of
advanced capabilities.  The following sections illustrate various combinations
of the possibilities.

.. toctree::
   :maxdepth: 2

   cons_traces
   cons_checkpoints

.. index::
   single: Coverage Consolidation

