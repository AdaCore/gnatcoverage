***********************************************************
Source/Object Coverage : Common Considerations & Facilities
***********************************************************

.. _exemptions:

Exemption Regions
=================

In some circumstances, there are good and well understood reasons why proper
coverage of some source construct is not achievable, and it is convenient to
be able to abstract these coverage violations away from the genuine defects of
a testing campaign.  The |gcp| :dfn:`exemptions` facility was designed for
this purpose.

For Ada with the |gnat| compilers, coverage exemptions are requested for
sections of source by the insertion of dedicated pragmas:

- ``pragma Annotate (Xcov, Exempt_On, "justification text");`` starts a
  section, providing some justification text that will be recalled in coverage
  reports.

- ``pragma Annotate (Xcov, Exempt_Off);`` closes the current exemption section.

There may be no overlap between exemption regions.

Exempted regions are reported as blocks in both the annotated source and the
synthetic text reports, for both source and object coverage metrics.

In annotated source reports, a ``#`` or ``*`` caracter annotates all the
exempted lines, depending on whether 0 or at least 1 violation was exempted
over the whole section, respectively.

In synthetic text reports, a single indication is emitted for each exempted
region, and the indications for all the regions are grouped in a separate
report section, only present if there are exemption regions in the analysis
scope.

This *Exempted violations* section lists and counts the exempted regions,
displaying for each the source location span, the number of actually exempted
violations in the region, and the exemption justification text. For example:

::

  =========================
  == 3. EXEMPTED REGIONS ==
  =========================

  assert.adb:22:4-27:4: 2 exempted violations, justification:
  assertions are expected never to fail

  1 exempted region.

In addition, the regular Coverage Violations sections gets renamed as
"NON-EXEMPTED COVERAGE VIOLATIONS".
  
.. index::
   single: Coverage Consolidation

.. _consolidation:

Coverage Consolidation
======================

Coverage consolidation is the |gcp| facility allowing the computation of the
overall coverage achieved by a set of executions. Consolidation is queried by
passing the corresponding set of execution traces to |gcvcov|, which produces
a single coverage report as a result. The focus of the analysis must be
specified, via :ref:`--scos <sunits>` for source coverage or :ref:`--routines
<oroutines>` for object coverage.

A typical case where consolidation is useful is when some part of an
application depends on external inputs and several executions with different
input sets are required to exercise different scenarii in the application
program. |gcp| supports this kind of use just fine, where the execution traces
to consolidate are obtained from the same executable.

|gcp| supports another kind of situation as well, where consolidation is
queried to compute the coverage achieved by different executables with
possibly overlapping symbols. This is typically useful with unit testing
campains, when different programs are built to exercise differents aspects of
a common application part.

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

      procedure Stat (Safe : Boolean);
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

         Result : constant Boolean
           := Cmd = Hold or else Front = Room;
      begin
         Stat (Result);
         return Result;
      end Safe;

   end Commands;

We test the Commands package body by combining two sorts of drivers: one
exercising cases where the ``Safe`` function is expected to return True, and
one for cases where the function is expected to return False.

The following code is a possible way to express the ``Safe`` returns True
expectations:

.. code-block:: ada

   procedure Test_Cmd_Safe is
   begin
      --  Remaining still is always safe, as well as stepping
      --  forward with room ahead

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
everything except the parts specific to *safe* situations.

Now, the combination of the two drivers was intended to achieve a pretty
complete testing of the provided functionality, and the corresponding coverage
can be computed thanks to the |gcp| consolidation facility. 

This is performed by simply providing the two execution traces to |gcvcov|,
for example like::

  gnatcov coverage --level=stmt --scos=commands.ali --annotate=xcov
     test_cmd_safe.trace test_cmd_unsafe.trace

Which confirms full statement coverage of the Commands package body:

.. code-block:: ada

 100% of 7 lines covered
 Coverage level: stmt
   1 .: package body Commands is
  .....
   6 .:    procedure Stat (Safe : Boolean) is
   7 .:    begin
   8 +:       if Safe then
   9 +:          N_Safe := N_Safe + 1;
  10 .:       else
  11 +:          N_Unsafe := N_Unsafe + 1;
  12 .:       end if;
  13 .:    end Stat;
  .....
  27 .: end Commands;

The performed consolidation indeed involved different programs with only
partial unit and object code overlap, as depicted on the following
representation::

    < Prog. 1 (test_cmd_safe)  >
    oooooooooooooooooooooooooooo-----------------+
    | Test_Cmd_Safe | Commands | Test_Cmd_Unsafe |
    +---------------oooooooooooooooooooooooooooooo
                    <  Prog. 2 (test_cmd_unsafe) >
                
The example analysis focused on the Commands unit for a source coverage
criterion. Of course, the other units could have been included in the analysis
as well, even though not overlapping between the different executable
programs.

Consolidation actually doesn't *require* any overlapping at all. The only
technical requirement is that the object code be identical for all the
overlapping symbols, which |gcp| verifies.

How does it get visible ? list of traces, tags, ...

similar for object coverage


.. _osmetrics:

Object vs Source level metrics
==============================

Even though the executable object code reflects semantics originally expressed
in the application sources, Object and Source level coverage metrics are of
very different nature, concerned with entities of very different kinds
(machine instructions vs high level constructs). This section's purpose is to
to illustrate this point further with a few observable differences in the
|gcp| outputs.

