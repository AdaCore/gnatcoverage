*************************************
Source/Object Coverage Considerations
*************************************

Even though the executable object code reflects semantics originally expressed
in the application sources, Object and Source level coverage metrics are of
very different nature, concerned with entities of very different kinds
(machine instructions vs high level constructs).

The first section of this chapter aims at stressing this point further with a
couple of illustrative examples. The following sections then present advanced
tools capabilities aimed at helping the analysis in various operational
contexts, when some coverage violations are legitimate and need to be documented,
or when the global assessment strategy needs consolidation across multiple programs
or executions.

.. _osmetrics:

Object vs Source level metrics
==============================

This section's purpose is to illustrate a few differences observable in the
|gcp| annotated source outputs, It is *not* to perform a qualitative
comparison between the two categories of criteria, way beyond the scope of
this toolset user guide. The essential point is twofold:

- Stress further that annotated source reports for object level criteria
  remain focused on object level metrics, and that source representations are
  just a means to organize and present the results in this case.

- Illustrate the |gcp| capability to offer accurate results for both kinds of
  criteria.

The first example we will look at is exercising the following functional
Ada unit:

.. code-block:: ada

   function Divides (X, Y : Integer) return Boolean;
   --  Whether X divides Y (as Y mod X is 0), outputing a message
   --  on standard output when True

   function Divides (X, Y : Integer) return Boolean is
   begin
      if Y mod X = 0 then
         Put_Line (Integer'Image (X) & " divides " & Integer'Image (Y));
         return True;
      else
         return False;
      end if;
   end Divides;

Using the basic test driver below:

.. code-block:: ada

   procedure Test_Ops1  is
   begin
      Assert (Divides (2, 4));
      Assert (not Divides (2, 5));
   end Test_Ops1;

``Divides`` features a simple decision controlling an *if* statement exercised
both ways so the driver achieves statement and decision coverage. It even
actually achieves MCDC coverage since the decision has a single condition, and
this is correctly reported by |gcp|, with 100% stmt+mcdc coverage and ``+``
annotations everywhere in the :option:`=xcov` output::

  gnatcov coverage --level=stmt+mcdc --scos=@alis --annotate=xcov test_ops1.trace

  # yields the following ops.adb.xcov:

  100% of 4 lines covered
  Coverage level: stmt+mcdc
   1 .: with Ada.Text_IO; use Ada.Text_IO;
   2 .:
   3 .: package body Ops is
   4 .:
   5 .:    function Divides (X, Y : Integer) return Boolean is
   6 .:    begin
   7 +:       if Y mod X = 0 then
   8 +:          Put_Line (Integer'Image (X) & " divides " & Integer'Image (Y));
   9 +:          return True;
  10 .:       else
  11 +:          return False;
  12 .:       end if;
  13 .:    end Divides;
  14 .:
  15 .: end Ops;

If we consider object coverage now, we have to consider that the Ada ``mod``
operator needs special treatment to handle negative operands, which incurs an
internal test (conditional branch) and dedicated sequences of
instructions. The operation normally also entails a check to raise the
predefined Contraint_Error exception if X happens to be null. These sequences
are not exercised by our basic driver, and object coverage for the same
execution trace correctly reports partial achievement only::

  gnatcov coverage --level=insn --annotate=xcov test_ops1.trace

  ...
  67% of 6 lines covered
  Coverage level: insn
  ...
   5 +:    function Divides (X, Y : Integer) return Boolean is
   6 .:    begin
   7 !:       if Y mod X = 0 then
   8 !:          Put_Line (Integer'Image (X) & " divides " & Integer'Image (Y));
   9 +:          return True;
  10 .:       else
  11 +:          return False;
  12 .:       end if;
  13 +:    end Divides;

Another difference we can notice here is the presence of coverage annotations
on lines 5 and 13, which had ``.`` in the source coverage reports. This
materializes the fact that there is machine code associated with these lines
(prologue and epilogue sequences, in particular), but no entity of source
level relevance (what we call :term:`Source Coverage Obligation`) at all there.

The second example we look at is the canonical case which exposed that object
branch coverage does not necessarily imply mcdc coverage, contrary to what was
believed for long:

.. code-block:: ada

   function Orand (A, B, C : Boolean) return Boolean is
   begin
      return (A or else B) and then C;
   end Orand;


The binary decision diagram of the decision, sketched below, is not a tree::

       f      f
    A ---> B ---> False
    |t     |t
    |      v  f
    +----> C ---> False
           |t
           +----> True

The simple driver below exercises all the paths through this BDD:

.. code-block:: ada

   procedure Test_Orand  is
      X : constant Boolean := True;
   begin
      Assert (Orand (True, X, True) = True);
      Assert (Orand (False, False, X) = False);
      Assert (Orand (False, True, False) = False);
   end Test_Orand;

Which is expected to achieve object branch coverage. As we will be comparing
with the mcdc assessment, we pass --scos and --level to |gcvrun| prior to
anything else, so we will be able to reuse the same execution trace for both
our object and source level experiments::

  gnatcov run --scos=@alis --level=stmt+mcdc test_orand

Now we verify that |gcp| correctly reports full object coverage, as expected::

   gnatcov coverage --level=branch --annotate=xcov test_orand.trace

   # yields orand.adb.xcov:

   100% of 3 lines covered
   Coverage level: branch

   1 +: function Orand (A, B, C : Boolean) return Boolean is
   2 .: begin
   3 +:    return (A or else B) and then C;
   4 +: end Orand;

With 3 tests for 3 conditions, mcdc cannot be achieved yet and |gcp| reports
this correctly as well. Using :option:`=xcov+` to see the reason for partial
coverage attached to line 3, we indeed get::

   gnatcov coverage --level=stmt+mcdc --scos=@alis --annotate=xcov+ test_orand.trace

   0% of 1 lines covered
   Coverage level: stmt+mcdc

   1 .: function Orand (A, B, C : Boolean) return Boolean is
   2 .: begin
   3 !:    return (A or else B) and then C;
   CONDITION "B" at 3:22 has no independent influence pair, MC/DC not achieved
   4 .: end Orand;

We have a clear illustration of the |gcp| ability to perform accurate
assessments of distinct source and object criteria here, actually based on
solid theoretical grounds established as part of the *Couverture* research
project from which |gcp| originates.

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

**Example**

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

**Further considerations**

In our example, the performed consolidation involved different programs with
only partial unit and object code overlap, as depicted on the following
representation::

    < test_cmd_safe executable >
    oooooooooooooooooooooooooooo-----------------+
    | Test_Cmd_Safe | Commands | Test_Cmd_Unsafe |
    +---------------oooooooooooooooooooooooooooooo
                    < test_cmd_unsafe executable >

The example analysis focused on the Commands unit for a source coverage
criterion. Of course, the other units could have been included in the analysis
as well, even though not overlapping between the different executable
programs.

Consolidation actually doesn't *require* overlapping. You might as well, for
example, want to consolidate results from different programs testing entirely
disjoint units. The only technical requirement is that the object code be
identical for all the overlapping symbols, which |gcp| verifies.

The set of traces involved in a computation is visible in various places:

- In the *Assessment Context* section of :option:`=report` outputs, where
  the command line is quoted and detailed information about each trace is
  provided (trace file name, timestamp, tag, ...)

- In the :option:`html` index header, where the list of trace names and tags
  used to produce the report is provided.

All the principles we have described so far apply to consolidated *object*
coverage analysis as well, and the only process differences are the general
source/object coverage ones. In particular, the focus of the analysis needs to
be specified with :ref:`--routines <oroutines>` instead of :ref:`--scos
<sunits>`, providing object level symbol names instead of source level unit
names.
