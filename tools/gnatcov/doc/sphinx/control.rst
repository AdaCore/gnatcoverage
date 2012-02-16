**************************
Trace Control and Contents
**************************

.. _trace-control:

Trace control for MCDC
=======================

MCDC analysis using execution traces requires specific care to make
sure that assessments are both accurate and efficient.
With |gcp|, this is achieved by the combination of two options passed
to |gcvrun|:

* :option:`--level=stmt+mcdc` to activate the collection of object branch
  histories, chronological record of the directions taken at conditional
  branch points in the machine code,

* :option:`--scos=@list-file` to convey the set of SCOs that will be subject
  to MCDC analysis, asking |gcv| to focus the branch history collections
  on the critical branches only.

MCDC assessment is only accurate when branch history was turned on for
decisions that require it, which |gcv| knows to determine from SCOs.  With
:option:`--level=stmt+mcdc` and in absence of :option:`--scos`, history is
activated for all the object conditional branch instructions, resulting in
larger traces and increased processing time compared to what is strictly
needed. Providing SCOs instructs |gcv| to restrict history collections to
branches that need it, allowing optimized operation downstream.  Care must be
taken in this case not to query MCDC analysis on SCOs that were not included
in the set provided to |gcvrun|.

Statement or decision coverage assessments, conversely, can be performed with
any kind of trace, so traces with history aimed at MCDC may be used for those
other criteria as well.

Trace file contents
===================

A trace file essentially consists in

* A header with general information about the trace generation context (name
  of the binary executable passed to |gcvrun|, :option:`--tag` argument value,
  production date & time, ...), followed by

* The machine execution trace entries (roughly, one per execution basic block,
  with information on the branch decision at the end)

The precise structure is described in the ``qemu_traces.ads`` unit of the
gnatcov sources, and gnatcov offers a :option:`dump-trace` option to display
the contents of trace files passed as arguments.

