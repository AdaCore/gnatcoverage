*******************
Trace File Contents
*******************

A trace file essentially consists in

* A header with general information about the trace generation context (name
  of the binary executable passed to |gcvrun|, :option:`--tag` argument value,
  production date & time, ...), followed by

* The machine execution trace entries (roughly, one per execution basic block,
  with information on the branch decision at the end)

|gcv| offers a :option:`dump-trace` option to display the contents of trace
files passed as arguments, displaying tags passed to |gcvrun| amongst other
things. For example::

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

Prior to the execution traces per-se (list of executed instruction blocks past
the ``Traces:`` label), we see a few information entries aimed at helping
|gcp| and users on various accounts. Each entry has a tag identifying it's
kind, then some associated data dependent on the kind. Our example above
features the following information entries:

``DATE_TIME`` :
  The trace production time stamp, always 8 bytes long.

``EXEC_FILE_NAME`` :
  Path to the binary program that was executed to produce the trace. This path
  can be exactly the one that was passed to |gcvrun| or a derived path, for
  instance if |gcvrun| had to add an extension to find the actual file (see
  :ref:`target_specific_notes`). |gcvcov| uses this path to find the program
  file again at analysis time, to find which machine code corresponds to which
  address for example.

``USER_DATA`` :
  User string tag for this trace, when one was passed with :option:`-T`
  to |gcvrun|.

The precise structure is described in the ``qemu_traces.ads`` unit of the
gnatcov sources. 

