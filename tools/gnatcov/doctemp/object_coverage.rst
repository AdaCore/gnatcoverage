************************
Object Coverage Analysis
************************

.. _ocov-principles:

General principles & Compilation requirements
=============================================

Object coverage analysis computes metrics focused on machine-level object
code, concerned with machine basic instructions or conditional branches.

On request, the metrics can be presented on sources, with an
annotation on each line synthesizing the coverage status of all the
instructions generated for this line. This mapping relies on debug
information, so sources must be compiled with :option:`-g` for this to
work. There is no further compilation requirement for object coverage
alone. However, if source coverage analysis is to be performed as
well, the whole process is simpler if the same compilation options are
used, and these have to be strictly controlled for source
coverage. See the :ref:`corresponding section <scov-principles>` for
more details.

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

  gnatcov coverage --level=<criterion> --annotate=<format> [--routines=<names>] ... <traces>

The optional :option:`--routines` argument provides the set of object level
subprogram names on which the analysis should focus. It defaults to the full
set of symbols defined by all the executables associated with the provided
execution traces.

The following sections now describe the :ref:`available report formats
<oreport-formats>`, then provide more details regarding the supported coverage
criteria and on the way to :ref:`specify the object routines of interest
<oroutines>`.

.. _oreport-formats:

Output report formats (:option:`--annotate`)
============================================

Machine level reports (:option:`=asm`)
--------------------------------------

For object coverage analysis, :option:`--annotate=asm` produces annotated
assembly code for all the selected routines on standard output.  The
annotations are first visible as a special character on each machine code line
to convey the coverage status of the corresponding instruction, for example::

   _assert !: fff00258-fff002b7
   fff00258 +:  94 21 ff e0      stwu   r1,-0x0020(r1)
   fff0025c +:  7c 08 02 a6      mflr   r0
   fff00260 +:  90 01 00 24      stw    r0,0x0024(r1)
   ...
   fff00280 +:  2f 80 00 00      cmpiw  cr7,r0,0x0000
   fff00284 +:  41 9e 00 18      beq-   cr7,fff0029c <support__assert+044>
   ...
   fff00294 -:  38 80 00 1b      li     r4,0x001b
   fff00298 -:  4b ff ff 35      bl     <__gnat_last_chance_handler>
   fff0029c +:  60 00 00 00      ori    r0,r0,0x0000
   ...
   fff002b4 +:  4e 80 00 20      blr

``-`` on a line always conveys that the instruction was not executed at all,
or (stated differently) *uncovered*. ``+`` means that the instruction is
*fully covered* with respect to the analyzed criterion and other annotations,
conveying *partial coverage* might show up depending on the criterion and kind
of instruction. More details on the instruction specific annotations are
provided in the criterion specific sections that follow.

As the example above suggests, the report also annotates each subprogram
symbol as a whole. The annotation includes the range of addresses that the
subprogram spans and a synthetic coverage indication:

.. csv-table::
  :delim: |
  :widths: 10, 80
  :header: Annotation, Meaning

   ``-`` | All the subprogram instructions are uncovered (none executed)
   ``+`` | All the subprogram instructions are fully covered
   ``!`` | Some of the subprogram instructions were fully or partially covered


Annotated sources, text (:option:`=xcov[+]`)
--------------------------------------------

For object coverage analysis, :option:`--annotate=xcov` produces annotated
source files with the ``.xcov`` extension in the current directory, one per
original compilation unit.

The annotations are visible as a special character at the beginning of
every source line, which synthesizes the coverage status of all the
machine instructions generated for this line.

We defined a uniform synthesis of source line from object code annotations for
both instruction and branch coverage:

.. csv-table::
  :delim: |
  :widths: 10, 80
  :header: Annotation, Meaning

   ``.`` | no machine code associated with this line
   ``-`` | all the instructions associated with the line are ``-`` (uncovered)
   ``+`` | all the instructions associated with the line are ``+`` (fully covered)
   ``!`` | otherwise

To lines with associated object code we apply qualifiers similar to those for
individual instructions: when the synthetic coverage indication for a line is
``-``, ``+`` or ``!``, we say qualify the line as *uncovered*, *fully
covered*, or *partially covered*, respectively.

Note that eventhough the annotations are rendered on source lines in this
case, they are really meant to convey object code properties, hence are of a
different nature than what the DO-178B source structural coverage criteria
refer to.

With an extra ``+`` at the end of the format name
(:option:`--annotate=xcov+`), the machine instructions and their individual
coverage status are printed next to their associated source line.

Example here

Annotated sources, html (:option:`=html[+]`)
--------------------------------------------

:option:`--annotate=html` produces one ``.html`` browsable annotated
source file per original compilation unit, in the current directory by
default or in the output directory selected with :option:`--output-dir`.

The annotations are identical to the :option:`=xcov` ones, and each source
line is colorized in green, orange and red to reflect full, partial or null
coverage respectively.

An ``index.html`` page summarizes the coverage results and provide
links to the annotated sources.

With the ``+`` extension, the annotated machine code for each line
may be expanded below it by a mouse click on the line.

Violations summary, text (:option:`=report`)
--------------------------------------------

For object coverage analysis, :option:`--annotate=report` produces a
synthetic summary of per function coverage results, with a single
annotation assigned to each function in the same way it is to each
source line in the *=xcov* or *=html* cases.

Object Instruction Coverage analysis (:option:`--level=insn`)
=============================================================

Object *Intruction* Coverage treats basic and conditional branch instructions
identically, as either executed (then fully covered) or not (then
uncovered). The :option:`=asm` instruction annotations follow:

.. csv-table::
  :delim: |
  :widths: 10, 80
  :header: Annotation, Meaning

   ``-`` | the instruction was not executed
   ``+`` | the instruction was executed

Example with conditional branch + even though not taken


Object Branch Coverage analysis (:option:`--level=branch`)
==========================================================

Object *Branch* Coverage treats basic and conditional branch instructions
differently. Basic instructions are considered fully covered as soon as
executed, while conditional branches have to be executed at least twice, once
taking the branch and once not taking it (executing fall-through) for this
purpose. To simplify the descriptions, we 

.. csv-table::
  :delim: |
  :widths: 10, 80
  :header: Annotation, Meaning

   ``-`` | the instruction was never executed
   ``+`` | the instruction was executed and taken both ways for a conditional branch
   ``>`` | the instruction is a conditional branch, executed and always taken
   ``v`` | the instruction is a conditional branch, executed and never taken

To illustrate, we will consider the Branch Coverage outcome for a piece
of the Explore example, produced out of a couple of runs within |qemu| for
the PowerPC architecture.
The original source of interest is the `if` statement which
controls the Station processing termination, upon a Quit request
from the user.
The control is performed by a single decision, composed by two connected
conditions to expose a case insensitive interface:


::

     procedure Run (Sta : Station_Access) is
        ...
        Put ("'P'robe, 'S'tep, Rotate 'L'eft/'R'ight, 'Q'uit ? ");
        Flush;
        Get (C);

        if C = 'Q' or else C = 'q' then
           Kill (Sta.all);
           return;
        else
        ...

  

We first run a sample session to exercise Probe, then Quit with 'Q',
and request branch coverage data in assembly format:


::

  ... $ gnatcov run --target=powerpc-elf explore
  [Explore runs in |qemu| - type 'p', then 'Q']

  ... $ gnatcov coverage --level=branch --annotate=asm explore.trace      

  

For the code associated with the source bits of interest, this yields
the following assembly coverage report excerpt:


::

  ...
  <stations__run>:
  ...
  fffc1c0c +:     4b ff e6 7d   bl     0xfffc0288 <text_io__get>
  fffc1c10 +:     2f 83 00 51   cmpiw  cr7,r3,0x0051
  fffc1c14 +:     41 9e 00 0c   **beq-**   cr7,0xfffc1c20 <stations__run+00000078>
  fffc1c18 +:     2f 83 00 71   cmpiw  cr7,r3,0x0071
  fffc1c1c >:     40 9e 00 10   **bne-**   cr7,0xfffc1c2c <stations__run+00000084>
  fffc1c20 +:     7f e3 fb 78   or     r3,r31,r31
  fffc1c24 +:     4b ff e7 d1   bl     0xfffc03f4 <actors__kill>
  ...
  

The `beq` and `bne` instructions are two conditional branches
corresponding to the two conditions.
In addition to straightforward coverage of the rest of the code, the '+'
for the first branch indicates that it is fully covered and the '>' for
the second branch indicates partial coverage only.
Indeed, both conditions were evaluated to False on the 'p' input, then
on 'Q' the first condition was evaluated to True and the second one was
short-circuited.

We run a second experiment, when the user quits with 'Q' immediatly.
We observe that the first conditional branch is only partially covered
and the second one is not even exercised:


::

  ...
  <stations__run>:
  ...
  fffc1c0c +:     4b ff e6 7d   bl     0xfffc0288 <text_io__get>
  fffc1c10 +:     2f 83 00 51   cmpiw  cr7,r3,0x0051
  fffc1c14 >:     41 9e 00 0c   beq-   cr7,0xfffc1c20 <stations__run+00000078>
  fffc1c18 -:     2f 83 00 71   cmpiw  cr7,r3,0x0071
  fffc1c1c -:     40 9e 00 10   bne-   cr7,0xfffc1c2c <stations__run+00000084>
  fffc1c20 +:     7f e3 fb 78   or     r3,r31,r31
  fffc1c24 +:     4b ff e7 d1   bl     0xfffc03f4 <actors__kill>
  ...
  

Inlined and Template/Generic entities
=====================================

The generated code for an inlined subprogram call or a generic
instantiation materializes two distinct source entities: the expanded
source (subprogram or package body) and the expansion request (subprogram
call or generic instanciation).

For inlined calls, the :command:`gcc` debug information associates the
generated machine code with the inlined source positions, so the related
object coverage information is attached there.  This scheme has all the
instances reported at a centralized location and allows use of the full
inlined subprogram source structure to organize the results.

Consider for example the following excerpt of branch coverage report for
the Station control code in Explore.
A call to an `Update` subprogram is inlined in
`Process_Pending_Inputs`.
We observe that the code reported in the `Update` sources is coming
from the `process_pending_inputs` symbol, where it was inlined, and
that absence of code is reported at the call site, since indeed all the
code for this call is attached to the inlined entity.


::

    53 .:       procedure Update (Map : in out Geomap; Situ : Situation) is
    54 +:          Posa : constant Position := Pos_Ahead_Of (Situ);
  <stations__run__process_pending_inputs.1939+fffc1bb4>:+
  fffc1c04 +:  4b ff ed c1  bl     0xfffc09c4 <geomaps__pos_ahead_of>
  fffc1c08 +:  90 61 00 30  stw    r3,0x0030(r1)
    55 .:       begin
    56 +:          Map (Posa.X, Posa.Y) := Situ.Sqa;
  <stations__run__process_pending_inputs.1939+fffc1bc4>:+
  fffc1c28 +:  88 01 00 19  lbz    r0,0x0019(r1)
  fffc1c2c +:  98 03 00 0f  stb    r0,0x000f(r3)
    [...]
    63 +:       procedure Process_Pending_Inputs (Sta : Station_Access) is
    [...]
    68 .:             Update (Sta.Map, Situ);
  

Similar principles apply to template instantiations such as those of Ada
generic units, and the centralized view property is well illustrated
this way.
The excerpt below provides an example with the `Queues` abstraction
in Explore, instantiated in several places.
The corresponding code sequences are all attached to original unit
source, with an indication of their instantiation locations via the
symbol names in the start-of-sequence addresses:


::

    39 +:    function Empty (Q : Queue) return Boolean is
  <robot_control_links__data_queue_p__empty+fffc02fc>:+
  fffc02fc +:  94 21 ff f0  stwu   r1,-0x0010(r1)
   [...]
  <geomaps__situation_links__data_queue_p__empty+fffc0878>:+
  fffc0878 +:  94 21 ff f0  stwu   r1,-0x0010(r1)
   [...]
  

.. _oroutines:

Focusing on subprograms of interest
===================================

|gcp| provides a number of facilities to allow filtering results so that
only those of actual interest show up.

The primary filtering device for object coverage analysis is the
:option:`--routines` option to :command:`gnatcov coverage`.

:option:`--routines` expects a single argument, to designate a set of
symbols, and restricts coverage results to machine code generated for
this set. The argument is either a single symbol name or the name of a file
prefixed with a |code| character, expected to contain a list of
symbol names.

To illustrate, the example command below produces a branch coverage
report for the `Unsafe` subprogram part of the `Robots` unit
in Explore.
Out of a |gnat| compiler, the corresponding object symbol name is
`robots__unsafe`, here designated by way of a single entry in a
symbol list file:


::

  $ cat slist
  robots__unsafe

  $ gnatcov coverage --level=branch --annotate=asm --routines=@slist explore.trace
  Coverage level: BRANCH
  robots__unsafe !: fffc1074-fffc109b
  fffc1074 +:  2f 83 00 02      cmpiw  cr7,r3,0x0002
  fffc1078 +:  40 be 00 1c      bne+   cr7,0xfffc1094 <robots__unsafe+00000020>
  [...]
  

|gcp| provides a *disp-routines* command to help the elaboration
of symbol lists.

The general synopsis is as follows::

   disp-routines :term:`[--exclude|--include] FILES`
     Build a list of routines from object files

`gnatcov disp-routines` outputs the list of symbols in a set built
from object files provided on the command line.
'Object file' is to be taken in the general sense of 'conforming to a
supported object file format, such as ELF', so includes executable files
as well as single compilation unit objects.

The output set is built incrementally while processing the arguments
left to right.
:option:`--include` states "from now on, symbols defined in the
forthcoming object files are to be added to the result set".
:option:`--exclude` states "from now on, symbols defined in the
forthcoming object files are to be removed from the result set".
An implicit :option:`--include` is assumed right at the beginning, and each
object file argument may actually be an :term:`@listfile` containing a list
of object files.

Below are a few examples of commands together with a description of the
set they build.


::

  $ gnatcov disp-routines explore
    # (symbols defined in the 'explore' executable)

  $ gnatcov disp-routines explore --exclude test_stations.o
    # (symbols from the 'explore' executable)
    # - (symbols from the 'test_stations.o' object file)

  $ gnatcov disp-routines --include @sl1 --exclude @sl2 --include @sl3
    # (symbols from the object files listed in text file sl1)
    # - (symbols from the object files listed in text file sl2)
    # + (symbols from the object files listed in text file sl3)

  

In-source reports, when requested, are generated for sources associated
with the selected symbols' object code via debug line information.
Coverage synthesis notes are produced only on those designated lines.
For example, `--annotate=xcov --routines=robots__unsafe` will
produce a single `robots.adb.xcov` in-source report with
annotations on the `Unsafe` function lines only, because the debug
info maps the code of the unique symbol of interest there and only there.

Note that inlining can have surprising effects in this context, when the
machine code is associated with the inlined entity and not the call
site.
When the code for a symbol A in unit Ua embeds code inlined from unit
Ub, an in-source report for routine A only will typically produce two
output files, one for Ua where the source of some of the symbol code
reside, and one for Ub, for lines referenced by the machine code inlined
in A.   

@page

