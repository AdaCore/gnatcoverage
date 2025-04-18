
.. _target_specific_notes:

Target specific points of note with Binary traces
=================================================

The following list summarizes points of note for each target where some
aspects of the build/execution/analysis process depart from the general
instructions.

For **cross** configurations in general:

- Need to convey the target to |gcvrun|, either with a :cmd-option:`--target`
  switch or a ``Target`` project file attribute;

- Need to convey the target and Ada runtime when using project files
  to state units of interest, for |gcvcov|, |gcvins| or |gcvrun|. See
  :ref:`sunits`, :ref:`gpr_context`.

For **powerpc-vxworks6**:

- Need to compile with -gno-strict-dwarf -mlongcall for source coverage
  analysis;

- Need to add a --kernel argument on |gcvrun|;

- The provided kernel has to be augmented with a specific module
  for gnatcov purposes. Please refer to the |gem| documentation for this
  part of the process;

- Support for the ``kernel`` Ada RTS and Downloadable Kernel Modules only.

.. _known_limitations:

Known Limitations with Binary traces
====================================

Incomplete statement coverage on lines with multiple statements
---------------------------------------------------------------

On lines with multiple statements, the tool may not be able to infer
accurate statement coverage results for each individual statement. The
tool emits explicit diagnostics in this case.

In :cmd-option:`=report` outputs (with :cmd-option:`--annotate=report`), this
translates as a statement coverage violation like::

 <sloc>: multiple statements on line, unable to establish full statement coverage

where <sloc> is a source-filename:line:column source location of the
problematic line.

In annotated sources kinds of outputs, this materializes as '!' note indicating
partial coverage on the affected lines, with the :cmd-option:`=report` violation
text quoted above available as part of the line extra-details expanded on demand
(:cmd-option:`=html` and :cmd-option:`xcov+` output formats).

.. _mcdc-limitations:

MCDC inaccuracies with interrupts and multi-threaded applications
-----------------------------------------------------------------

There is one limitation in |gcp| with respect to MCDC assessments from binary
traces: potential inaccuracies in results reported for particular decisions
when these decisions are evaluated concurrently by different threads or mixed
with interrupt processing in bareboard configurations.

Technically, the decisions of concern are those for which the associated
binary decision diagram is not a tree, that is, those with at least one
condition node joining several possible evaluation paths.

The code sample below illustrates the simplest possible problematic decision
and the following figure depicts the corresponding Binary Decision Diagram
(commonly abbreviated as *BDD*), which states how sequence of operand
evaluations, starting from the left, eventually lead to the expression
outcome, here on the right:

.. code-block:: ada

  function Mp (A, B, C : Boolean) return Boolean is
  begin
    return (A or else B) and then C;
  end;

.. figure:: fig_multipath-bdd.*
  :align: center

  BDD for ``(A or else B) and then C``, not a tree

The expression BDD is indeed not a tree, as the node representing the
evaluation of C is reachable either directly from A, when A is True, or
via B when A is False.

According to measures performed on a few large real code bases, occurrences of
such decisions are statistically rare.  |gcv| can report about them on demand,
thanks to the :command:`scan-decisions` command together with the the set of
coverage obligations to examine. Below is an excerpt of a an execution for a
project which encompasses this function, where we see that |gcv| provides the
source location of conditions reachable through multiple paths::

  gnatcov scan-decisions -Pmytest.gpr
  ...
  *** mp.adb:4:33: warning: condition is reachable through multiple paths
