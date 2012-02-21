**************************
Trace Contents and Control
**************************

Trace file contents
===================

A trace file essentially consists in

* A header with general information about the trace generation context (name
  of the binary executable passed to |gcvrun|, :option:`--tag` argument value,
  production date & time, ...), followed by

* The machine execution trace entries (roughly, one per execution basic block,
  with information on the branch decision at the end)

The precise structure is described in the ``qemu_traces.ads`` unit of the
gnatcov sources. gnatcov offers a :option:`dump-trace` option to display
the contents of trace files passed as arguments, displaying tags passed to
|gcvrun| amongst other things. For example::

   gnatcov dump-trace test_divmod2.trace

   Tag  : DATE_TIME (Date)
   Len  : 8
   Data : dc 07 02 15 08 00 25 00
          2012-02-21 08:00:37

   Tag  : EXEC_FILE_NAME
   Len  : 16
   Data : obj/test_divmod2

   Tag  : USER_DATA (User_Tag)
   Len  : 10
   Data : sample tag

   Traces:
   fffffffc-fffffffb ?: 20 ---- fault
   fffffffc-ffffffff ?: 11 ---t block
   fff0067c-fff006b3 ?: 11 ---t block
   fff006bc-fff006bf ?: 12 --t- block
   [...]

Indeed, prior to the execution traces per-se (list of executed instruction
blocks past the ``Traces:`` label), we see a few information entries aimed at
helping |gcp| and users on various accounts. Each entry has a tag identifying
it's kind, then some associated data dependent on the kind. Our example above
features the following information entries:

``DATE_TIME`` :
  The trace production time stamp, always 8 bytes long.

``EXEC_FILE_NAME`` :
  Path to the binary probgram that was executed to produce the trace. This
  path is exactly the one that was passed to |gcvrun| and which |gcvcov| uses
  to find the program file again at analysis time, to find which machine code
  corresponds to which address for example.

``USER_DATA`` :
  User string tag for this trace, when one was passed with :option:`-T`
  to |gcvrun|.

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

