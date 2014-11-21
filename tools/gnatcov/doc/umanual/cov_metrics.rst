.. _osmetrics:

******************************************
Object/Source level metrics considerations
******************************************

Even though the executable code reflects semantics expressed in the
application sources, Object and Source level coverage metrics are of very
different nature, concerned with machine instructions vs high level constructs
respectively.

Our purpose here is to illustrate this through a few examples, *not* to
perform a qualitative comparison between the two kinds of criteria, way beyond
the scope of this toolset user guide. The essential point is twofold:

- Stress that annotated source reports for object criteria remain focused on
  object level metrics, and that source representations are just a means to
  present the results in this case.

- Illustrate the |gcp| ability to compute accurate results for both kinds of
  criteria.

Main differences examplified
============================

To illustrate the main differences between the two kinds of metrics, we
exercise the following functional Ada unit:

.. code-block:: ada

   --  Return whether X divides Y, print a message when True

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
achieves mcdc since the decision has a single condition, which is reported by
|gcp| with 100% stmt+mcdc coverage and ``+`` annotations everywhere in the
:option:`=xcov` output::

  gnatcov coverage --level=stmt+mcdc --scos=@alis --annotate=xcov test_ops1.trace
  ...
  100% of 4 lines covered
  Coverage level: stmt+mcdc
  ...
   5 .:    function Divides (X, Y : Integer) return Boolean is
   6 .:    begin
   7 +:       if Y mod X = 0 then
   8 +:          Put_Line (Integer'Image (X) & " divides " & Integer'Image (Y));
   9 +:          return True;
  10 .:       else
  11 +:          return False;
  12 .:       end if;
  13 .:    end Divides;

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

Full branch coverage vs MCDC
============================

The second example we look at is the canonical case which exposed that object
branch coverage does not necessarily imply mcdc coverage, contrary to what was
believed for long. Consider this source and the associated decision Binary
Decision Diagram:

.. code-block:: ada

   function Orand (A, B, C : Boolean) return Boolean is
   begin
      return (A or else B) and then C;
   end Orand;

.. _fig-multipath-bdd:
.. figure:: multipath-bdd.*
  :align: center

  BDD for ``(A or else B) and then C``

The simple driver below exercises all the paths through this BDD:

.. code-block:: ada

   procedure Test_Orand  is
      X : constant Boolean := True;
   begin
      Assert (Orand (True, X, True) = True);
      Assert (Orand (False, False, X) = False);
      Assert (Orand (False, True, False) = False);
   end Test_Orand;

As we will be comparing with the mcdc assessment, we pass :option:`--scos` and
:option:`--level` to |gcvrun| prior to anything else, so we will be able to
reuse the same execution trace for both our object and source level
experiments::

  gnatcov run --scos=@alis --level=stmt+mcdc test_orand

Now we verify that |gcp| reports full object coverage as expected::

   gnatcov coverage --level=branch --annotate=xcov test_orand.trace
   ...
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
   ...
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
project from which |gcp| originates. The core particularity allowing full
branch coverage without mcdc is the presence of decisions with BDDs which
are not trees, as we have in this specfic case,

