.. index::
   single: Coverage Consolidation

.. _consolidation:

**********************
Coverage Consolidation
**********************

Coverage consolidation is the |gcp| facility allowing the computation of the
overall coverage achieved by a set of executions. Consolidation is queried by
passing the corresponding set of execution traces to |gcvcov|, which produces
a single coverage report as a result. The focus of the analysis must be
specified, via :ref:`--scos` or project files for source coverage, or via
:ref:`--routines <oroutines>` for object coverage.

A typical case where consolidation is useful is when some part of an
application depends on external inputs and several executions are required to
exercise different scenarii in the application program. |gcp| supports this
kind of use just fine, where the execution traces to consolidate are obtained
from the same executable.

|gcp| supports another kind of situation as well, where consolidation is
queried to compute the coverage achieved by different executables with
possibly overlapping symbols. This is typically useful with unit testing
campains, when different programs are built to exercise differents aspects of
a common application part.

Introductory Example
====================

We will consider achieving statement coverage of the following example Ada
units to illustrate:

.. code-block:: ada

   package Commands is
      type Command is (Step, Hold);
      type Perceived is (Room, Rock, Pit);

      function Safe (Cmd : Command; Front : Perceived) return Boolean;
      --  Whether executing CMD is safe with FRONT perceived ahead

      N_Safe, N_Unsafe : Integer := 0;
      --  Count the number of safe/unsafe cases we have evaluated
   end Commands;

   package body Commands is

      --  Update our eval counters according to a SAFE evaluation just made
      procedure Stat (Safe : Boolean) is
      begin
         if Safe then
            N_Safe := N_Safe + 1;
         else
            N_Unsafe := N_Unsafe + 1;
         end if;
      end Stat;

      function Safe (Cmd : Command; Front : Perceived) return Boolean is
         --  Standing straight is always safe, and any other action is
         --  safe as soon as there is room ahead.
         Result : constant Boolean := Cmd = Hold or else Front = Room;
      begin
         Stat (Result);
         return Result;
      end Safe;
   end Commands;

We test the Commands package body by combining two sorts of drivers. The first
one exercises cases where the ``Safe`` function is expected to return True:

.. code-block:: ada

   procedure Test_Cmd_Safe is
   begin
      --  Remaining still is always safe, as is stepping with room ahead
      Assert (Safe (Cmd => Hold, Front => Rock));
      Assert (Safe (Cmd => Hold, Front => Pit));
      Assert (Safe (Cmd => Step, Front => Room));
   end Test_Cmd_Safe;

Running the first program and analysing the achieved coverage for this one
alone would be something like::

  gnatcov run test_cmd_safe   # produces test_cmd_safe.trace
  gnatcov coverage --level=stmt --scos=commands.ali --annotate=xcov test_cmd_safe.trace

Producing a ``commands.adb.xcov`` report with:

.. code-block:: ada

   6 .:    procedure Stat (Safe : Boolean) is
   7 .:    begin
   8 +:       if Safe then
   9 +:          N_Safe := N_Safe + 1;
  10 .:       else
  11 -:          N_Unsafe := N_Unsafe + 1;
  12 .:       end if;
  13 .:    end Stat;

In accordance with the testcase strategy, aimed at exercising *safe*
situations only, everything is statement covered except the code specific to
*unsafe* situations, here the counter update on line 11.
Now comes the other driver, exercising cases where the ``Safe`` function is
expected to return False:

.. code-block:: ada

   procedure Test_Cmd_Unsafe is
   begin
      --  Stepping forward without room ahead is always unsafe
      Assert (not Safe (Cmd => Step, Front => Rock));
      Assert (not Safe (Cmd => Step, Front => Pit));
   end Test_Cmd_Unsafe;

This one alone produces the symetric ``commands.adb.xcov`` report, with:

.. code-block:: ada

   6 .:    procedure Stat (Safe : Boolean) is
   7 .:    begin
   8 +:       if Safe then
   9 -:          N_Safe := N_Safe + 1;
  10 .:       else
  11 +:          N_Unsafe := N_Unsafe + 1;
  12 .:       end if;
  13 .:    end Stat;

There again, the coverage results are in accordance with the intent, testing
everything except the parts specific to *safe* situations.  The combination of
the two drivers was intended to achieve a pretty complete testing of the
provided functionality, and the corresponding coverage can be computed thanks
to the |gcp| consolidation facility, by simply providing the two execution
traces to |gcvcov|, which indeed yields full statement coverage of the
Commands package body::

  gnatcov coverage [...] test_cmd_safe.trace test_cmd_unsafe.trace
  ...

.. code-block:: ada

   6 .:    procedure Stat (Safe : Boolean) is
   7 .:    begin
   8 +:       if Safe then
   9 +:          N_Safe := N_Safe + 1;
  10 .:       else
  11 +:          N_Unsafe := N_Unsafe + 1;
  12 .:       end if;
  13 .:    end Stat;


Further use cases
=================

In our example, the performed consolidation involved different programs with
only partial unit and object code overlap, as depicted on the following
representation:

.. _fig-consolidation:
.. figure:: consolidation.*
  :align: center

  Overlapping executables
  
The example analysis focused on the Commands unit for a source coverage
criterion. The other units may be included in the analysis as well, even
though not overlapping between the different executables.

Consolidation actually doesn't *require* overlapping: users might well, for
example, consolidate results from different programs testing entirely disjoint
sets of units. A typical situation where this would happen is when testing
independant units of a library.

Overlap processing during consolidation
=======================================

For object or source level criteria, |gcv| computes the coverage achieved for
the full set of routines or source units declared to be of interest amongst
those exposed by the union of the exercised executables, as designated by the
set of consolidated traces;

On symbols found to overlap across executables, |gcv| computes the *combined*
coverage achieved by all the executions.

For the purpose of computing combined coverage achievements, two symbols are
considered overlapping when all the following conditions are met:

* Both symbols have identical object level symbol names,

* Both symbols have DWARF debug information attached to them,

* According to this debug information, both symbols originate from the same
  compilation unit, denoted by the full path of the corresponding source file.

By construction, a symbol missing debug information is never considered
overlapping with any other symbol. Whatever coverage is achieved on such a
symbol never gets combined with anything else and the only kind of report
where the symbol coverage is exposed is the :option:`=asm` assembly output for
object level criteria.

Moreover, for object level coverage criteria, |gcvcov| will issue a
consolidation error when two symbols are found to overlap but have
structurally different machine code, which happens for example when the same
unit is compiled with different different optimization levels for
different executables.

The set of traces involved in a computation is visible in various places:

- In the *Assessment Context* section of :option:`=report` outputs, where
  the command line is quoted and detailed information about each trace is
  provided (trace file name, timestamp, tag, ...)

- In the :option:`=html` index page, where the list of trace names and tags
  used to produce the report is provided.

