************************
Object Coverage Analysis
************************

General principles & Compilation requirements
=============================================

Object coverage analysis computes metrics focused on machine-level object
code, concerned with machine basic instructions or conditional branches.

On request, the metrics can be presented on sources, with an annotation on
each line synthesizing the coverage status of all the instructions
generated for this line. This mapping relies on debug information, so
sources must be compiled with :option:`-g` for this to work. There is no
further compilation requirement for object coverage alone. However, if
source coverage analysis is to be performed as well, the whole process is
simpler if the same compilation options are used, and these have to be
strictly controlled for source coverage.

Once your application is built, the analysis proceeds in two steps:
|gcvrun| is used to produce execution traces, then |gcvcov| to generate
coverage reports. *Object* coverage is queried by passing a specific
:option:`--level` argument. The possible values for source level analysis
are :option:`insn`, and :option:`branch`, described in detail in later
sections of this documentation.

As for source coverage, there is never a requirement to recompile just
because a different criterion needs to be analyzed.

The :ref:`gnatcov_run-commandline` section of this document provides details on
the trace production interface. The remainder of this chapter explains the use
of |gcvcov| in particular, to analyse traces once they have been produced.

The following sections now describe the available report formats, then
provide more details and examples regarding the supported coverage criteria.


.. _oreport-formats:

Output report formats
=====================

Machine level reports, `--annotate=asm`
---------------------------------------

For object coverage analysis, :option:`--annotate=asm` produces annotated assembly
code for all the program routines on standard output.  The annotations are
visible as a special character at the beginning of each machine code line
to convey information about the corresponding instruction, with variants
for instruction or branch coverage modes.

  We call @dfn:term:`simple` those
machine instructions which are not @dfn:term:`conditional branch`
instructions.

For @dfn:term:`Object Instruction Coverage`, with *--level=insn*, we
define:

@multitable @columnfractions .1 .8
* @h:term:`Note` @tab @h:term:`Means ...`
* '`-`'
@tab instruction was never executed
* '`+`'
@tab instruction was executed
@end multitable

For @dfn:term:`Object Branch coverage` (*--level=branch*),
the `+' case is refined for conditional branch instructions and two
additional notes are possible:

@multitable @columnfractions .1 .8
* @h:term:`Note` @tab @h:term:`Means ...`
* '`-`'
@tab instruction never was executed
* '`+`'
@tab instruction was executed, taken both ways for a conditional
branch
* '`>`'
@tab conditional branch was executed, always taken
* '`v`'
@tab conditional branch was executed, never taken
@end multitable

We qualify instructions marked with *+* as @dfn:term:`fully covered`,
those marked with *-* as @dfn:term:`uncovered` and the others as
@dfn:term:`partially covered`.

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
  

Annotated sources, text : :option:`--annotate=xcov[+]`
------------------------------------------------------

For object coverage analysis, *--annotate=xcov* produces
annotated source files with the `.xcov` extension in the current
directory, one per original compilation unit.

The annotations are visible as a special character at the beginning of
every source line, which synthesizes the coverage status of all the
machine instructions generated for this line.

The machine instructions are printed next to their associated source
line when the *+* option extension is used.
Eventhough the annotations are rendered on source lines in this case,
they are really meant to convey object code properties, hence are of a
different nature than what the DO-178B structural coverage criteria
refer to.

We defined a uniform synthesis of source line from object code
annotations for both instruction and branch coverage:

@multitable @columnfractions .1 .8
* @h:term:`Note` @tab @h:term:`Means ...`
* '`.`'
@tab no machine code associated with this line
* '`-`'
@tab all the instructions associated with the line are '-' (uncovered)
* '`+`'
@tab all the instructions associated with the line are '+' (fully covered)
* '`!`'
@tab otherwise
@end multitable

To lines with associated object code we apply qualifiers similar to
those for individual instructions: '-', '+' and '!' denote
@dfn:term:`uncovered`, @dfn:term:`fully covered` or @dfn:term:`partially covered` lines
respectively.

At this stage, |gcv| relies on dwarf debug information to associate
machine instructions with their corresponding source lines, so these
annotations are only possible when this is available.
In |gcc| parlance, this requires compilation with the *-g*
command line switch, designed never to influence the generated code.

Annotated sources, html : :option:`--annotate=html[+]`
------------------------------------------------------

*--annotate=html* produces one `.html` browsable annotated
source file per original compilation unit in the current directory.
The annotations are identical to the *=xcov* ones, and an
alternate output directory may be selected with *--output-dir*
as well.
Each source line is colorized to reflect its associated object code
coverage completeness, with green, orange and red for full, partial or
null coverage respectively.

An `index.html` page summarizes the coverage results and provide
links to the annotated sources.
With the `+` extension, the annotated machine code for each line
may be expanded below it by a mouse click on the line.

Violations summary, text : :option:`--annotate=report`
------------------------------------------------------

For object coverage analysis, *--annotate=report* produces a
synthetic summary of per function coverage results, with a single
annotation assigned to each function in the same way it is to each
source line in the *=xcov* or *=html* cases.

Inlined and Template/Generic entities
=====================================

The generated code for an inlined subprogram call or a generic
instantiation materializes two distinct source entities: the expanded
source (subprogram or package body) and the expansion request (subprogram
call or generic instanciation).

For inlined calls, the |gcc| debug information associates the generated
machine code with the inlined source positions, so the related object
coverage information is attached there.
This scheme has all the instances reported at a centralized location and
allows use of the full inlined subprogram source structure to organize
the results.

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
  

Focusing on subprograms of interest
===================================

|gcp| provides a number of facilities to allow filtering results so that
only those of actual interest show up.

The primary filtering device for object coverage analysis is the
*--routines* option to `gnatcov coverage`.
*--routines* expects a single argument, to designate a set of
symbols, and restricts coverage results to machine code generated for
this set.
The argument is either a single symbol name or the name of a file
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
The general synopsis is as follows:


::

  @verbatim
   disp-routines :term:`[--exclude|--include] FILES`
     Build a list of routines from object files
  @end verbatim
  

`gnatcov disp-routines` outputs the list of symbols in a set built
from object files provided on the command line.
'Object file' is to be taken in the general sense of 'conforming to a
supported object file format, such as ELF', so includes executable files
as well as single compilation unit objects.

The output set is built incrementally while processing the arguments
left to right.
*--include* states ``from now on, symbols defined in the
forthcoming object files are to be added to the result set''.
*--exclude* states ``from now on, symbols defined in the
forthcoming object files are to be removed from the result set''.
An implicit `--include` is assumed right at the beginning, and each
object file argument may actually be an |code|file containing a list
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

