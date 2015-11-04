**********
Appendices
**********

.. _sample_sc_html_index:

Sample html index
=================

.. image:: sample_sc_html_index.png
   :scale: 80%

.. _sample_sc_html_unit:

Sample html annotated source
============================

.. image:: sample_sc_html_unit.png
   :scale: 80%


.. _target_specific_notes:

Target specific points of note
==============================

The following list summarizes points of note for each target where some
aspects of the build/execution/analysis process depart from the general
instructions.

For **powerpc-vxworks6**:

- Need to compile with -gno-strict-dwarf for source coverage analysis;

- Need to add a --kernel argument on |gcvrun|;

- The provided kernel has to be augmented with a specific module
  for gnatcov purposes. Please refer to the |gem| documentation for this
  part of the process;

- Support for the ``kernel`` Ada RTS and Downloadable Kernel Modules only.

For **Windows native**:

- One can omit the extension for the program passed to |gcvrun|. So for
  instance on this target, ``gnatcov run foo`` is equivalent to ``gnatcov run
  foo.exe``.  In any case, unless the ``-o`` option is passed, the name for the
  generated trace file is based on the actual program file name, not the one
  passed to |gcvrun|. So both previous examples will generate ``foo.exe.trace``
  files. Besides, both will leave "foo.exe" as the executable name stored in
  the trace header.

.. _known_limitations:

Known Limitations
=================

Incomplete statement coverage on lines with multiple statements
---------------------------------------------------------------

On lines with multiple statements, the tool may not be able to infer
accurate statement coverage results for each individual statement. The
tool emits explicit diagnostics in this case.

In :option:`=report` outputs (with :option:`--annotate=report`), this
translates as a statement coverage violation like::

 <sloc>: multiple statements on line, unable to establish full statement coverage

where <sloc> is a source-filename:line:column source location of the
problematic line.

In annotated sources kinds of outputs, this materializes as '!' note
indicating partial coverage on the affected lines, with the :option:`=report`
violation text quoted above available as part of the line extra-details
expanded on demand (:option:`=html+` and :option:`xcov+` output formats).

.. _mcdc-limitations:

MCDC inaccuracies with interrupts and multi-threaded applications 
-----------------------------------------------------------------

There is one limitation in |gcp| with respect to MCDC assessments: potential
inaccuracies in results reported for particular decisions when these decisions
are evaluated concurrently by different threads or mixed with interrupt
processing in bareboard configurations.

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

.. figure:: multipath-bdd.*
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
source location of conditions rechable through multiple paths::

  gnatcov scan-decisions -Pmytest.gpr
  ...
  *** mp.adb:4:33: warning: condition is reachable through multiple paths

