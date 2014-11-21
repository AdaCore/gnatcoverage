.. index::
   single: Coverage Consolidation

.. _consolidation:

**********************
Coverage Consolidation
**********************

Coverage consolidation is the facility allowing the computation of the overall
coverage achieved by a set of executions. Consolidation is queried by passing
the corresponding set of execution traces to |gcvcov|, which produces a single
coverage report as a result. The focus of the analysis must be specified, via
:ref:`--scos` or project files for source coverage, or via :ref:`--routines
<oroutines>` for object coverage.

A typical case where consolidation is useful is when some part of an
application depends on external inputs and several executions are required to
exercise different scenarios in the application program. The execution traces
to consolidate are obtained from the same executable in this case.

Another common situation is when execution of different executables is needed
to achieve the required coverage for a software, either because distinct
software modules are tested independently (e.g. the different units of a
library), or because different aspects of the behavior of modules are
tested separately (e.g. the different subprograms of a library unit or
different scenarios of a given subprogram).

A simple example is provided for each of these cases in the following
sections.

Example 1: consolidation over a single program
==============================================

Consider the example C program below, offering a simple command line interface
to perform very basic math operations. This is splitted in two main source
files: ``process.c`` doing the computation and displaying the result, and
``main.c`` for the main entry point and basic usage control:

.. code-block:: c

   #include <stdio.h>        /* main.c */
   #include <assert.h>
   #include "process.h"

   void usage ()
   {
     printf ("calc <int1> <int2> <op>, print result of <int1> <op> <int2>\n");
   }

   int main (int argc, const char * argv[])
   {
     if (argc != 4)
       {
         usage ();
         exit (1);
       }

     process (argv);
     return 0;
   }


.. code-block:: c

   #include <stdio.h>        /* process.c */
   #include <assert.h>
   #include "process.h"

   void process (const char * argv[])
   {
     int x = atoi (argv[1]), y = atoi (argv[2]);
     char opcode = argv[3][0];

     int result;

     switch (opcode)
       {
       case '*':
         result = x * y;
         break;
       case '+':
         result = x + y;
         break;
       default:
         printf ("unsupported opcode %c\n", opcode);
         return;
       }

     printf ("%d %c %d = %d\n", x, opcode, y, result);  
   }  


.. code-block:: c

   #ifndef __PROCESS_H__     /* process.h */
   #define __PROCESS_H__
   extern void process (const char * argv[]);
   #endif


Here is a sequence of compilation/executions for various use cases, on a
native system where command line arguments for the program are supported by
|gcvrun|. Each execution is requested to produce a specific trace file::

   gcc -o calc main.c process.c -g -fpreserve-control-flow -fdump-scos
   gnatcov run --output=mult.trace -eargs ./calc 6 5 '*'             
   gnatcov run --output=plus.trace -eargs ./calc 2 3 '+'  
   gnatcov run --output=div.trace -eargs ./calc 2 3 '/'  
   gnatcov run --output=misuse.trace -eargs ./calc 

Now we can use |gcvcov| to assess the coverage achieved by arbitrary
combinations of the executions, just by passing the corresponding traces.
For example, combining the two executions exercising the ``*`` and ``+``
computations for statement coverage can be achieved with::

   gnatcov coverage --scos=main.c.gli --scos=process.c.gli \
      --annotate=xcov --level=stmt mult.trace plus.trace

And this yields reports in ``main.c.xcov`` and ``process.c.xcov`` like::

   ...
   5 .: void usage ()
   6 .: {
   7 -:   printf ("calc <i1> <i2> <op>, print result of <i1> <op> <i2>\n");
   8 .: }
   9 .: 
  10 .: int main (int argc, const char * argv[])
  11 .: {
  12 +:   if (argc != 4)
  13 .:     {
  14 -:       usage ();
  15 -:       exit (1);
  16 .:     }
  17 .: 
  18 +:   process (argv);
  19 +:   return 0;
  20 .: }

   ...
   5 .: void process (const char * argv[])
   6 .: {
   7 +:   int x = atoi (argv[1]), y = atoi (argv[2]);
   8 +:   char opcode = argv[3][0];
   9 .: 
  10 +:   int result;
  11 .: 
  12 +:   switch (opcode)
  13 .:     {
  14 .:     case '*':
  15 +:       result = x * y;
  16 +:       break;
  17 .:     case '+':
  18 +:       result = x + y;
  19 +:       break;
  20 .:     default:
  21 -:       printf ("unsupported opcode %c\n", opcode);
  22 -:       return;
  23 .:     }
  24 .:   
  25 +:   printf ("%d %c %d = %d\n", x, opcode, y, result);  
  26 .: }  


We observe a reported absence of coverage for statements corresponding to the
treatment of two kinds of usage error: wrong number of command line arguments,
visible on lines 7, 14, and 15 of ``main.c``, and attempt to compute an
unsupported operation, visible on lines 21 and 22 of ``process.c``. These two
scenarios, exercised through ``div.trace`` and ``misuse.trace`` were indeed
not included in the consolidation scope.


Example 2: consolidation over a single unit by different programs
==================================================================

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

We test the ``Commands`` package body by combining two sorts of drivers. The
first one exercises cases where the ``Safe`` function is expected to return
True:

.. code-block:: ada

   procedure Test_Cmd_Safe is
   begin
      --  Remaining still is always safe, as is stepping with room ahead

      Assert (Safe (Cmd => Hold, Front => Rock));
      Assert (Safe (Cmd => Hold, Front => Pit));
      Assert (Safe (Cmd => Step, Front => Room));
   end Test_Cmd_Safe;

Running this first program and analysing the achieved coverage would be
something like::

  gnatcov run test_cmd_safe   # produces test_cmd_safe.trace

  gnatcov coverage --level=stmt --scos=commands.ali --annotate=xcov test_cmd_safe.trace

Producing a ``commands.adb.xcov`` report with::

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

This one alone produces the symmetric ``commands.adb.xcov`` report, with::

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

   6 .:    procedure Stat (Safe : Boolean) is
   7 .:    begin
   8 +:       if Safe then
   9 +:          N_Safe := N_Safe + 1;
  10 .:       else
  11 +:          N_Unsafe := N_Unsafe + 1;
  12 .:       end if;
  13 .:    end Stat;


In this example, consolidation involved different programs with only partial
object code overlap, as depicted on the following representation:

.. _fig-consolidation:
.. figure:: consolidation.*
  :align: center

  Overlapping executables
  
Consolidation actually doesn't *require* overlapping: users might well, for
example, consolidate results from different programs testing entirely disjoint
sets of units. A typical situation where this would happen is when testing
independent units of a library, as illustrated by the following example.

Example 3: consolidation over a library by different programs
=============================================================

This example is a nice opportunity to illustrate a possible use of project
files to denote the units of interest, so we'll provide more details on that
aspect. Let us consider an example library composed of the following two Ada
procedures, implemented in separate source files ``inc.adb`` and ``mult.adb``:

.. code-block:: ada

  procedure Inc (X : in out Integer; Amount : Integer) is   -- inc.adb
  begin
     X := X + Amount;
  end;

  procedure Mult (X : in out Integer; Amount : Integer) is  -- mult.adb
  begin
     X := X * Amount;
  end;


We first build an archive library from these, using the *gprbuild* tool (part
of the GNAT toolchain). We place the two sources in a ``libops`` subdirectory
and use the ``libops.gpr`` example project file below at the toplevel::

   library project Libops is

      for Library_Dir use "lib";     -- Request creation of lib/libops.a
      for Library_Kind use "static";
      for Library_Name use "ops";

      for Languages use ("Ada");     -- Sources are Ada, in libops/ subdir
      for Source_Dirs use ("libops");
      for Object_Dir use "obj";

      package Compiler is
         for default_switches ("Ada") use
            ("-g", "-fdump-scos", "-fpreserve-control-flow");
      end compiler;

   end Libops;

``gprbuild -Plibops`` builds the library with the proper compilation options,
then we move on to unit tests. We write two different programs for this
purpose:

.. code-block:: ada

   with Inc, Assert;     -- test_inc.adb
   procedure Test_Inc is
      X : Integer := 0;
   begin
      Inc (X, 1);
      Assert (X = 1);
   end;

   with Mult, Assert;    -- test_mult.adb
   procedure Test_Mult is
      X : Integer := 2;
   begin
      Mult (X, 2);
      Assert (X = 4);
   end;


We build the corresponding executables using gprbuild again, with the
``test.gpr`` project file below::

   with "libops";  -- test.gpr

   project Test is
     for Languages use ("Ada");
     for Object_Dir use "obj";

     package Compiler is
       for Default_Switches("Ada") use ("-fno-inline");
     end Compiler;
   end Test;

   gprbuild -Ptest.gpr test_inc.adb test_mult.adb

We're not interested in the coverage of the test procedures themselves so we
don't need the coverage related compilation options. :option:`-fno-inline` is
enforced to make sure that the library object code really gets exercised and
not an inlined version of it within the test harness.

Now we can run the tests and perform coverage analysis for various
combinations. To begin with::

   gnatcov run obj/test_inc   -- produces test_inc.trace

   gnatcov run obj/test_mult  -- produces test_mult.trace

Then assessing the library statement coverage achieved by the ``test_inc`` unit
test, say as a violations report, would go like::

  gnatcov coverage --level=stmt --annotate=report -Plibops test_inc.trace

Note the use of :option:`-Plibops` to state that the library units are those
of interest for our analysis, without having to specify the location of the
corresponding LI files. From the single provided trace, there's no reference
to the ``mult`` unit at all and all the statements therein are marked
uncovered in this case. We'd get::

   2.1. STMT COVERAGE
   ------------------

   mult.adb:3:4: statement not executed

   1 violation.

Proper coverage of the library units is achieved by the two unit tests,
which we can see  by requesting the consolidated coverage achieved by the
two executions::

   gnatcov coverage --level=stmt --annotate=report -Plibops test_*.trace
   ...
   2.1. STMT COVERAGE
   ------------------

   No violation.

Processing of object code overlap during consolidation
======================================================

For object or source level criteria, |gcv| computes the coverage achieved for
the full set of routines or source units declared to be of interest amongst
those exposed by the union of the exercised executables, as designated by the
set of consolidated traces;

On symbols found to overlap across executables, |gcv| computes the *combined*
coverage achieved by all the executions.

For the purpose of computing combined coverage achievements, two symbols are
considered overlapping when all the following conditions are met:

* Both symbols have identical names at the object level,

* Both symbols have DWARF debug information attached to them,

* According to this debug information, both symbols originate from the same
  compilation unit, denoted by the full path of the corresponding source file.

By this construction, a symbol missing debug information is never considered
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

