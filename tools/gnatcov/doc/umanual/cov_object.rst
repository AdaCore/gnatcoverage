.. _ocov:

************************
Object Coverage Analysis
************************

.. _ocov-principles:

General principles & Compilation requirements
=============================================

Object coverage analysis computes metrics focused on machine-level object
code, concerned with machine basic instructions or conditional branches.

On request, the metrics can be presented on sources, with an annotation on
each line synthesizing the coverage status of all the instructions generated
for this line. This mapping relies on debug information, so sources must be
compiled with :option:`-g` for this to work. There is no further compilation
requirement for object coverage alone. However, if :ref:`source coverage
analysis <scov>` is to be performed as well, the whole process is simpler if
the same compilation options are used and they have to be strictly controlled
for the source level criteria.

Once your application is built, the analysis proceeds in two steps: |gcvrun|
is used to produce execution traces, then |gcvcov| to generate coverage
reports. *Object* coverage is queried by passing a specific :option:`--level`
argument to |gcvcov|; :option:`=insn` or :option:`=branch`, described in
detail in the following sections. As for source coverage, there is never a
requirement to recompile just because a different criterion needs to be
analyzed.

The :ref:`gnatcov_run-commandline` section of this document provides details
on the trace production interface. The remainder of this chapter explains the
use of |gcvcov| in particular, to analyse traces once they have been
produced. The general command line structure is always like::

  gnatcov coverage --level=<criterion> --annotate=<format>
                   [--routines=<names>] ... <traces>

The optional :option:`--routines` argument provides the set of object level
subprogram names on which the analysis should focus. This set defaults to the
full set of symbols defined by all the executables associated with the
provided execution traces.

Later in this chapter, :ref:`oroutines` explains how to construct the relevant
list of names for :option:`--routines`.  Prior to this comes a section
describing the :ref:`available report formats <oreport-formats>`, then more
details regarding :ref:`ocov-insn`, :ref:`ocov-branch`, and specificities
regarding :ref:`ocov-generics`. Finally, :ref:`ocov-full` describes tools that
help analyze low-level object files for issues of interest when aiming at full
object coverage.

.. _oreport-formats:

Output report formats (:option:`--annotate`)
============================================

Object coverage reports may be produced in various formats, as requested with
the :option:`--annotate` command line argument of |gcvcov|.

The :option:`asm` format produces an annotated assembly output, with a
coverage indication attached to every single instruction. This is the base
information of interest to object coverage analysis, simply presented in
different manners through the other possible output formats. The
:option:`xcov` and :option:`html` formats both produce a set of annotated
source files, in the directory where |gcv| is launched unless overriden with a
:ref:`--output-dir option <cov-outdir>`. Even though presented on sources, the
annotations remain representative of object coverage metrics, synthesized for
all the instructions associated with each source line.

Later in this chapter we name output formats by the text to add to
:option:`--annotate` on the command line. For example, we use "the
:option:`=asm` outputs" to mean "the coverage reports produced with
:option:`--annotate=asm`". We also sometimes use *in-source* reports
or outputs to designate the set of outputs in annotated source forms. 

We illustrate the various formats with coverage analysis excerpts on
the following example Ada support unit:

.. code-block:: ada

   --  raise Program_Error if T is False. Do nothing otherwise.

   procedure Assert (T : Boolean) is
   begin
      if not T then
         raise Program_Error;
      end if;
   end Assert;

As the contents suggest, this subprogram is expected never to be called
with T False in nominal situations.

Machine level reports (:option:`=asm`)
--------------------------------------

For object coverage analysis, :option:`--annotate=asm` produces annotated
assembly code for all the selected routines on standard output.  The
annotations are first visible as a special character on each machine code line
to convey the coverage status of the corresponding instruction.

The following output excerpt, for example, is part of a coverage report for
our ``Assert`` subprogram compiled for the PowerPc architecture::

   Coverage level: branch
   _ada_assert !: 0c4-123
   0c4 +:  94 21 ff e0      stwu   r1,-0x0020(r1)
   ...
   0ec +:  2f 80 00 00      cmpiw  cr7,r0,0x0000
   0f0 +:  41 9e 00 18      beq-   cr7,0x108 <_ada_assert+00000044>
   ...
   100 -:  38 80 00 04      li     r4,0x0004
   104 -:  48 00 00 a1      bl     0x1a4 <__gnat_last_chance_handler>
   108 +:  60 00 00 00      ori    r0,r0,0x0000
   ...
   120 +:  4e 80 00 20      blr

A ``-`` annotation for a line always conveys that the instruction was not
executed at all. The instruction is also said to be *uncovered* in this
case. Conversely, a ``+`` means that the instruction is *fully covered* with
respect to the analyzed criterion, with a meaning which depends on both the
criterion and the kind of instruction -- typically, whether the instruction
is a conditional branch and whether we are doing mere instruction or object
branch coverage nalaysis.

Other annotations, conveying *partial coverage*, might show up as well, also
depending on the criterion and kind of instruction. More details on the
instruction specific annotations are provided in the criterion specific
sections that follow.

As the first line of the example suggests, the report also annotates each
subprogram symbol as a whole, with the range of addresses that the subprogram
spans and a synthetic coverage indication according to the following table:

.. tabularcolumns:: cl

.. csv-table::
  :delim: |
  :widths: 10, 80
  :header: Symbol Annotation,Meaning

   ``-`` | All the subprogram instructions are uncovered (none executed)
   ``+`` | All the subprogram instructions are fully covered
   ``!`` | Some of the subprogram instructions were fully or partially covered

In our example, the code features both fully covered and uncovered
instructions, and the ``_assert`` symbol as a whole is marked partially
covered with a ``!`` annotation.

Annotated sources, text (:option:`=xcov[+]`)
--------------------------------------------

For object coverage analysis, :option:`--annotate=xcov` produces annotated
source files with the ``.xcov`` extension, one per original compilation unit
in the :ref:`selected output directory <cov-outdir>`.

The annotations are visible at the beginning of every source line, as a
single character which synthesizes the coverage status of all the machine
instructions generated for this line. The following table povides a uniform
description of this synthesis for all the object level criteria:

.. tabularcolumns:: cl

.. csv-table::
  :delim: |
  :widths: 10, 80
  :header: Source Annotation, Meaning

   ``.`` | no machine code associated with this line
   ``-`` | all the instructions associated with the line are ``-`` (uncovered)
   ``+`` | all the instructions associated with the line are ``+`` (fully covered)
   ``!`` | otherwise

The report also includes a short header, which features a global coverage
count with respect to the total number of lines with associated code, as well
as an indication of the assessed criterion. Below is an example of report
obtained for our Assert unit:

.. code-block:: ada
 
 examples/src/assert.adb:
 75% of 4 lines covered
 Coverage level: insn
   1 +: procedure Assert (T : Boolean) is
   2 .: begin
   3 +:    if not T then
   4 -:       raise Program_Error;
   5 .:    end if;
   6 +: end Assert;


To *lines* with associated object code we apply qualifiers similar to those
for individual instructions: when the synthetic coverage indication for a line
is ``-``, ``+`` or ``!``, we qualify the line as *uncovered*, *fully covered*,
or *partially covered*, respectively. Note that even though they are rendered
on source lines, the annotations are really meant to convey object code
properties here, hence are of a different nature than what the DO-178B source
structural coverage criteria refer to. See our :ref:`osmetrics` section for
further details on this aspect.

With :option:`--annotate=xcov+` (extra ``+`` at the end), the machine
instructions and their individual coverage status are printed next to their
associated source line.

Annotated sources, html (:option:`=html[+]`)
--------------------------------------------

For object coverage criteria, |gcvcov| :option:`--annotate=html` produces an
annotated version of each source file, in html format, named after the original
source with an extra ``.html`` extension at the end.

Each annotated source page contains a summary of the assessment results
followed by the original source lines, all numbered and marked with a coverage
annotation as in the :option:`--annotate=xcov` case. In addition, lines with
obligations are colorized in green, orange or red for ``+``, ``!`` or ``-``
coverage respectively. An `index.html` page is also produced, which contains a
description of the assessment context (assessed criteria, set of trace files
involved, ...) and a summary of the coverage results for all the units, with
links to their annotated sources.

Similarily to the :option:`xcov` format case, :option:`--annotate=html+` (with
a trailing +) attaches to each line details about the coverage status of all
the individual instructions generated for the line. These are folded within
the line and expanded when a mouse click hits it.

The page style is governed by a set of Cascading Style Sheet (CSS) parameters,
fetched from a ``xcov.css`` file in the directory where |gcv| is launched. If
this file is available when |gcv| starts, |gcv| uses it so users may setup a
customized version if needed. If the file is not available, |gcv| creates a
default one.

.. _ocov-insn:

Object Instruction Coverage analysis (:option:`--level=insn`)
=============================================================

Object *Instruction* Coverage treats basic and conditional branch instructions
identically, as either executed or not, hence fully covered or uncovered. The
:option:`=asm` instruction annotations are as follows:

.. tabularcolumns:: cl

.. csv-table::
  :delim: |
  :widths: 10, 80
  :header: Insn Annotation, Meaning

   ``-`` | the instruction was not executed
   ``+`` | the instruction was executed

The :option:`=asm` excerpt below provides a representative example of the
PowerPC instruction coverage achieved for our ``Assert`` procedure by nominal
executions where the subprogram is never called with T False::

   _ada_assert !: 0c4-123
   0c4 +:  94 21 ff e0      stwu   r1,-0x0020(r1)
   ...
   0ec +:  2f 80 00 00      cmpiw  cr7,r0,0x0000
   0f0 +:  41 9e 00 18      beq-   cr7,0x108 <_ada_assert+00000044>
   ...
   100 -:  38 80 00 04      li     r4,0x0004
   104 -:  48 00 00 a1      bl     0x1a4 <__gnat_last_chance_handler>
   108 +:  60 00 00 00      ori    r0,r0,0x0000
   ...
   120 +:  4e 80 00 20      blr

Expectedely, the coverage annotations report all the instructions as executed
except the two issuing the call to ``__gnat_last_chance_handler``, which
correspond to the ``raise`` statement in the GNAT high integrity profiles
without exception propagation support. The two instructions at offsets 0ec and
0f0 are the comparison and branch conditioned on the comparison result that
implement the *if* construct. We note here that the conditional branch is
reported fully covered, as merely executed, even though always taken.

The corresponding :option:`=xcov` output follows:

.. code-block:: ada

   1 +: procedure Assert (T : Boolean) is
   2 .: begin
   3 +:    if not T then
   4 -:       raise Program_Error;
   5 .:    end if;
   6 +: end Assert;

The annotations on lines 3 and 4 correspond to immediate expectations from
comments we made on the :option:`=asm` output. We can also observe annotations
on lines 1 and 6, to which the subprogram prologue and epilogue code is
attached, and executed as soon as the procedure is called.

.. _ocov-branch:

Object Branch Coverage analysis (:option:`--level=branch`)
==========================================================

Object *Branch* Coverage treats basic and conditional branch instructions
differently. Basic instructions are considered fully covered as soon as
executed, as in the Instruction Coverage case.  Conditional branches, however,
have to be executed at least twice to be claimed fully covered : once taking
the branch and once executing fall-through, which we sometimes abusively refer
to as :dfn:`taken both ways` even if one case actually corresponds to the
branch not being taken.  The :option:`=asm` instruction annotations are as
follows:

.. tabularcolumns:: cl

.. csv-table::
  :delim: |
  :widths: 10, 80
  :header: Insn Annotation, Meaning

   ``-`` | the instruction was never executed
   ``+`` | the instruction was executed and taken both ways for a conditional branch
   ``>`` | the instruction is a conditional branch, executed and always taken
   ``v`` | the instruction is a conditional branch, executed and never taken

The ``v`` and ``>`` annotations are representative of situations where a
conditional branch instruction is executed and :dfn:`taken one way` only, which
constitutes *partial coverage* of the instruction.

An example of partial coverage is observable on our now familiar Assert case,
where the conditional branch at offset 0f0 is always taken, jumping over the
raise statement code as expected for nominal executions with all assertions
satisfied::

   _ada_assert !: 0c4-123
   0c4 +:  94 21 ff e0      stwu   r1,-0x0020(r1)
   ...
   0ec +:  2f 80 00 00      cmpiw  cr7,r0,0x0000
   0f0 >:  41 9e 00 18      beq-   cr7,0x108 <_ada_assert+00000044>
   ...
   100 -:  38 80 00 04      li     r4,0x0004
   104 -:  48 00 00 a1      bl     0x1a4 <__gnat_last_chance_handler>
   108 +:  60 00 00 00      ori    r0,r0,0x0000
   ...
   120 +:  4e 80 00 20      blr

The corresponding :option:`=xcov` output follows::

 examples/src/assert.adb:
 50% of 4 lines covered
 Coverage level: branch
   1 +: procedure Assert (T : Boolean) is
   2 .: begin
   3 !:    if not T then
   4 -:       raise Program_Error;
   5 .:    end if;
   6 +: end Assert;

The partial branch coverage logically translates into a partial coverage
annotation on the line to which the branch is attached, here the line of the
*if* statement that the conditional branch implements. This is confirmed by
the :option:`=xcov+` output, where the individual instructions are visible as
well together with their own coverage indications::

   examples/src/assert.adb:
   50% of 4 lines covered
   Coverage level: branch
      1 +: procedure Assert (T : Boolean) is
   <_ada_assert+00000000>:+
   0c4 +:  94 21 ff e0  stwu   r1,-0x0020(r1)
   ...
   0dc +:  98 1f 00 08  stb    r0,0x0008(r31)
      2 .: begin
      3 !:    if not T then
   <_ada_assert+0000001c>:!
   0e0 +:  88 1f 00 08  lbz    r0,0x0008(r31)
   ...
   0ec +:  2f 80 00 00  cmpiw  cr7,r0,0x0000
   0f0 >:  41 9e 00 18  beq-   cr7,0x108 <_ada_assert+00000044>
      4 -:       raise Program_Error;
   <_ada_assert+00000030>:-
   0f4 -:  3c 00 ff f0  lis    r0,-0x0010
   ...
   104 -:  48 00 00 a1  bl     0x1a4 <__gnat_last_chance_handler>
      5 .:    end if;
      6 +: end Assert;
   <_ada_assert+00000044>:+
   ...
   120 +:  4e 80 00 20  blr

.. _oroutines:

Focusing on subprograms of interest (:option:`--routines`)
==========================================================

By default, in absence of a :option:`--routines` argument to |gcvcov|, object
coverage results are produced for the full set of subprogram symbols defined
by the executables designated by the analyzed traces.

:option:`--routines` allows the specification of a set of subprogram symbols
of interest so reports refer to this (sub)set only.
Each occurrence of :option:`--routines` on the command line expects a single
argument which specifies a subset of symbols of interest. Multiple occurrences
are allowed and the subsets accumulate. The argument might be either a single
symbol name or a :term:`@listfile argument` expected to contain a list of
symbol names.

For example, focusing on three symbols ``sym1``, ``sym2`` and ``sym3`` can be
achieved with either one of the following set of :option:`--routines`
combinations::

  --routines=sym1 --routines=sym2 --routines=sym3
  or --routines=@symlist123
  or --routines=sym3 --routines=@symlist12

... provided a ``symlist12`` text file containing the first two symbol names 
and a ``symlist123`` text file containing the three of them.

It is often convenient to compute the lists of symbols for a :term:`@listfile
argument`, for example as "the full set of defined subprograms except those
with ``test_`` or ``harness_`` at the beginning of their name". |gcv| provides
the |gcvdsp| sub-command for this purpose.

The general synopsis of |gcvdsp| is as follows::

   disp-routines [--exclude|--include] FILES
     Build a list of routines from object files

|gcvdsp| outputs the list of symbols in a set built from :dfn:`object files`
provided on the command line. :dfn:`Object file` is to be taken in the general
sense here, as :dfn:`conforming to a supported object file format, typically
ELF`, so includes executable files as well as single compilation unit objects.

The output set is built incrementally while processing the arguments left to
right. :option:`--include` states "from now on, until contradicted, symbols
defined in object files are added to the result set", and :option:`--exclude`
states "from now on, until contradicted, symbols defined in object files are
removed from the result set". An implicit :option:`--include` is assumed right
at the beginning, and each argument may be either the direct name of an object
file or a :term:`@listfile argument` containing a list of such names.

Below are a few examples of commands together with a description of the
set they build::

  $ gnatcov disp-routines explore
    # (symbols defined in the 'explore' executable)

  $ gnatcov disp-routines explore --exclude test_stations.o
    # (symbols from the 'explore' executable)
    # - (symbols from the 'test_stations.o' object file)

  $ gnatcov disp-routines --include @sl1 --exclude @sl2 --include @sl3
    # (symbols from the object files listed in text file sl1)
    # - (symbols from the object files listed in text file sl2)
    # + (symbols from the object files listed in text file sl3)


Annotated source reports, when requested, are generated for sources associated
with the selected symbols' object code via debug information, and coverage
annotations are produced only on the corresponding lines.

For example, assuming we have a ``robots.adb`` Ada unit featuring a ``Reset``
subprogram, which produces a ``robots__reset`` object symbol::

  gnatcov coverage --level=insn --annotate=xcov --routines=robots__reset

... would produce a single `robots.adb.xcov` annotated source report with
annotations on the `Reset` subprogram lines only when the debug info maps the
code of the unique symbol of interest there and only there. Inlining can have
surprising effects in this context, as the following section describes in
greater details.

.. _ocov-generics:

Inlining & Generic units
========================

The generated code for an inlined subprogram call or a generic instantiation
materializes two distinct source entities: the expanded source (of the inlined
subprogram or of the instanciated generic body) and the expansion request (the
subprogram call or the generic instanciation). While this is of no consequence
for :option:`=asm` outputs, which just report coverage of raw machine
instructions within their object level subprograms, regardless of the object
code origin, this raises a few points of note for in-source outputs.

For inlined calls, potentially surprising results might show up when a
specific set of object routines is queried. Indeed, when the code for a symbol
A in unit Ua embeds code inlined from unit Ub, a request for an annotated
source report for routine A, intuitively expected to yield a report for Ua
only, will typically produce an output file for Ub as well, for lines
referenced by the machine code inlined in A.

Consider the following Ada units for example, in source files named
``intops.ads``, ``intops.adb`` and ``test_inc0.adb``:

.. code-block:: ada

   -- Functional unit

   package Intops is
      procedure Inc (X : in out Integer);
      pragma Inline (Inc);
   end Intops;

   package body Intops is
      procedure Inc (X : in out Integer) is
      begin
         X := X + 1;
      end Inc;
   end Intops;


   -- Test Driver

   procedure Test_Inc0  is
      X : Integer := 0;
   begin
      Inc (X);
      Assert (X = 1);
   end Test_Inc0;


Compiling so that the ``Inc (X);`` call in Test_Inc0 is inlined, and after
execution of the ``test_inc0`` executable, the following analysis::

  gnatcov coverage --level=insn --routines=_test_inc0 --annotate=xcov+ test_inc0.trace

Thanks to :option:`--routines`, this requests to report about the Test_Inc0
procedure only, so we intuitively expect a single ``test_inc0.adb.xcov``
annotated source result. The command actually produces an ``intops.adb.xcov``
report as well because the object code of Test_Inc0 also contains inlined
code coming from the other unit.

For generic units, |gcp| aggregates information for all the instances on the
common generic source, so each line annotation is sort of a super synthesis of
the coverage achieved for all the instructions attached to this line through
all the generic instances.

Let us consider the generic Ada unit below to illustrate:

.. code-block:: ada

   generic
      type Num_T is range <>;
   package Genpos is
      procedure Count (X : Num_T);
      --  Increment N_Positive is X > 0

      N_Positive : Natural := 0;
      --  Number of positive values passed to Count
   end Genpos;

   package body Genpos is
      procedure Count (X : Num_T) is
      begin
         if X > 0 then
            N_Positive := N_Positive + 1;
         end if;
      end Count;
   end Genpos;

Then two distinct instances in their own package, producing separate
object code for each instance:

.. code-block:: ada

   package POSI is
      type T1 is new Integer;
      package Pos_T1 is new Genpos (Num_T => T1);

      type T2 is new Integer;
      package Pos_T2 is new Genpos (Num_T => T2);
   end POSI;

And now a simple test driver that executes all the code for ``Count`` in the
first instance (going within the *if* statement), and only part of the code
for ``Count`` in the second instance (not going within the *if* statement):
   
.. code-block:: ada

   procedure Test_Genpos is
   begin
      Pos_T1.Count (X => 1);
      Assert (Pos_T1.N_Positive = 1);

      Pos_T2.Count (X => -1);
      Assert (Pos_T2.N_Positive = 0);
   end Test_Genpos;

The precise :option:`insn` coverage difference is first visible in the
:option:`=asm` report. The conditioned part of ``Count`` clearly shows up as
uncovered in the ``Pos_T2`` instance (``-`` at offset 204 and on), while it is
reported covered as expected in the ``Pos_T1`` instance (``+`` at offset 1b4
and on)::

   posi__pos_t1__count +: 1ac-1e7
   1ac +:  2f 80 00 00      cmpiw  cr7,r0,0x0000
   1b0 +:  40 9d 00 24      ble-   cr7,0x1d4 <posi__pos_t1__count+0000003c>
   1b4 +:  3c 00 00 00      lis    r0,0x0000        -
   ...                                              - cond branch not taken
   1d4 +:  60 00 00 00      ori    r0,r0,0x0000  <--o
   ...

   posi__pos_t2__count !: 1fc-237
   1fc +:  2f 80 00 00      cmpiw  cr7,r0,0x0000
   200 +:  40 9d 00 24      ble-   cr7,0x224 <posi__pos_t2__count+0000003c>
   204 -:  3c 00 00 00      lis    r0,0x0000        |
   ...                                              | cond branch taken
   224 +:  60 00 00 00      ori    r0,r0,0x0000 <---o
   ...

This yields a partial coverage annotation for the corresponding source line in
the :option:`=xcov` output (``!`` on line 10):

.. code-block:: ada

   6 .: package body Genpos is
   7 +:    procedure Count (X : Num_T) is
   8 .:    begin
   9 +:       if X > 0 then
  10 !:          N_Positive := N_Positive + 1;
  11 .:       end if;
  12 +:    end Count;
  13 .: end Genpos;

And the :option:`=xcov+` (or :option:`=html+`) output gathers everything
together, with the blocks of instructions coming from different instances
identifiable by the associated object symbol names::

     10 !:          N_Positive := N_Positive + 1;
   <posi__pos_t1__count+0000001c>:+
   1b4 +:  3c 00 00 00  lis    r0,0x0000
   ...
   1d0 +:  91 2b 10 48  stw    r9,0x1048(r11)
   <posi__pos_t2__count+0000001c>:-
   204 -:  3c 00 00 00  lis    r0,0x0000
   ...
   220 -:  91 2b 10 4c  stw    r9,0x104c(r11)


.. _ocov-full:

Full object coverage considerations
===================================

The previous sections focus on the coverage analysis of code attached to
*symbols*, as listed by |gcv| :option:`disp-routines`. When full object level
coverage is to be reached, a few extra details need to be looked at in
addition. In particular, care is required regarding:

* Orphaned code regions, that are not attached to any symbol and are
  unaddressed by regular coverage reports,

* Empty symbols, for which the reported code size is null.

Orphaned regions usually show up out of legitimate code alignment requests
issued for performance or target ABI considerations. Empty symbols most often
result from low level assembly programmed parts missing the assembly
directives aimed at populating the symbol table flags and fields.

Both cases are typically harmless and easy to deal with once identified and
analyzed, so information about them is only emitted on demand and not by
default in every coverage report for any object level criterion. |gcv|
provides the :option:`scan-objects` command to help there, which expects the
set of object files to examine on the command line, as a sequence of either
object file or :term:`@listfile argument`, and reports about the two kinds
of situations described above.


