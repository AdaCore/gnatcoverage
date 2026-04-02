******************************************************
Consolidation with varying source coverage obligations
******************************************************

Some projects need to provide alternative implementations depending on the
context: for instance different backends for some service, to be selected at
compilation time.

Here is a simple example: the ``Logging`` package provides helper to emit
errors or warnings. A single source file (``logging.ads``) defines the API to
emit these messages, but two source files (``logging__text_io.adb`` and
``logging__gnat_io.adb``) implement this API: one relies on the ``Ada.Text_IO``
runtime unit, and the other on the ``GNAT.IO`` unit.

.. code-block:: ada

   -- example.gpr
   project Example is
      type Logging_Backend_Type is ("text_io", "gnat_io");
      Logging_Backend : Logging_Backend_Type := external ("LOGGING_BACKEND");

      for Main use ("main.adb");
      for Object_Dir use "obj-" & Logging_Backend;

      package Naming is
         for Body ("logging") use "logging__" & Logging_Backend & ".adb";
      end Naming;
   end Example;

   -- logging.ads
   package Logging is
      procedure Warn (Message : String);
      procedure Error (Message : String);
   end Logging;

   -- logging__gnat_io.adb
   with GNAT.IO; use GNAT.IO;
   package body Logging is

      procedure Write (Prefix, Message : String) is
      begin
         Put (Prefix);
         Put (Message);
         New_Line;
      end Write;

      procedure Warn (Message : String) is
      begin
         Write ("warning: ", Message);
      end Warn;

      procedure Error (Message : String) is
      begin
         Write ("error: ", Message);
      end Error;
   end Logging;

   -- logging__text_io.adb
   with Ada.Text_IO; use Ada.Text_IO;
   package body Logging is

      procedure Write (Prefix, Message : String) is
      begin
         Put (Prefix);
         Put_Line (Message);
      end Write;

      procedure Warn (Message : String) is
      begin
         Write ("warning: ", Message);
      end Warn;

      procedure Error (Message : String) is
      begin
         Write ("error: ", Message);
      end Error;
   end Logging;
   with Logging;

   -- main.adb
   with Logging;
   procedure Main is
   begin
      Logging.Warn ("message");
   end Main;

|gcv| allows to compute code coverage for both implementations. They
must be instrumented separately so that all source files have a chance of being
instrumented:

.. code-block:: sh

   gnatcov instrument -Pexample --level=stmt -XLOGGING_BACKEND=text_io
   gnatcov instrument -Pexample --level=stmt -XLOGGING_BACKEND=gnat_io

Building and running the programs in order to generated source traces is next:

.. code-block:: sh

   gprbuild -Pexample -XLOGGING_BACKEND=text_io \
      --src-subdirs=gnatcov-instr --implicit-with=gnatcov_rts
   gprbuild -Pexample -XLOGGING_BACKEND=gnat_io \
      --src-subdirs=gnatcov-instr --implicit-with=gnatcov_rts

   GNATCOV_TRACE_FILE=main_text_io.srctrace obj-text_io/main
   GNATCOV_TRACE_FILE=main_gnat_io.srctrace obj-gnat_io/main

As described in :ref:`sunits`, coverage obligations for all the source files
that need to appear in the coverage report must be conveyed to ``gnatcov
coverage`` eventually. Since a given set of project loading options (``-P``,
``-X``) covers one alternative at a time, special care must be taken when
generating the coverage report. The two methods of consolidation can be usede
here:

1. Manually pass the relevant SID files (``--sid`` switch) to use all source
   traces at once (i.e. :ref:`cons-traces`).
2. Keep using project loading options to load source traces separately and
   produce checkpoint files (i.e. :ref:`checkpoints`).

Use SID files to use all source traces at once
==============================================

In order to make |gcvcov| aware of all source files referenced in source
traces, pass all the relevant SID files (:ref:`passing_scos`):

.. code-block:: sh

   gnatcov coverage \
     --sid obj-text_io/logging__text_io.sid \
     --sid obj-gnat_io/logging__gnat_io.sid \
     main*.srctrace \
     --level=stmt --annotate=xcov --output-dir=xcov

This yields the expected coverage report, mentionning the two alternative
implementations of ``Logging``:

.. code-block:: sh

   cat xcov/logging.ads.xcov
   [...]/logging.ads:
   no code
   no code
   Coverage level: stmt
      1 .:    package Logging is
      2 .:       procedure Warn (Message : String);
      3 .:       procedure Error (Message : String);
      4 .:    end Logging;

   cat xcov/logging__text_io.adb.xcov
   [...]/logging__text_io.adb:
   75% of 4 lines covered
   75% statement coverage (3 out of 4)

   Coverage level: stmt
      1 .:    with Ada.Text_IO; use Ada.Text_IO;
      2 .:    package body Logging is
      3 .:
      4 .:       procedure Write (Prefix, Message : String) is
      5 .:       begin
      6 +:          Put (Prefix);
      7 +:          Put_Line (Message);
      8 .:       end Write;
      9 .:
     10 .:       procedure Warn (Message : String) is
     11 .:       begin
     12 +:          Write ("warning: ", Message);
     13 .:       end Warn;
     14 .:
     15 .:       procedure Error (Message : String) is
     16 .:       begin
     17 -:          Write ("error: ", Message);
     18 .:       end Error;
     19 .:    end Logging;

   cat xcov/logging__gnat_io.adb.xcov
   [...]/logging__gnat_io.adb:
   80% of 5 lines covered
   80% statement coverage (4 out of 5)

   Coverage level: stmt
      1 .:    with GNAT.IO; use GNAT.IO;
      2 .:    package body Logging is
      3 .:
      4 .:       procedure Write (Prefix, Message : String) is
      5 .:       begin
      6 +:          Put (Prefix);
      7 +:          Put (Message);
      8 +:          New_Line;
      9 .:       end Write;
     10 .:
     11 .:       procedure Warn (Message : String) is
     12 .:       begin
     13 +:          Write ("warning: ", Message);
     14 .:       end Warn;
     15 .:
     16 .:       procedure Error (Message : String) is
     17 .:       begin
     18 -:          Write ("error: ", Message);
     19 .:       end Error;
     20 .:    end Logging;


Use project loading options to load source traces separately
============================================================

|gcvcov| must load each source trace with the corresponding set of project
loading options (``-P``, ``-X``): each time, we create a checkpoint to carry
the corresponding coverage information:

.. code-block:: sh

   gnatcov coverage -Pexample -XLOGGING_BACKEND=text_io \
     main_text_io.srctrace \
     --level=stmt --save-checkpoint=text_io.ckpt

   gnatcov coverage -Pexample -XLOGGING_BACKEND=gnat_io \
     main_gnat_io.srctrace \
     --level=stmt --save-checkpoint=gnat_io.ckpt

Now that each source trace was loaded into a checkpoint, it is possible to
generate the coverage report from the checkpoints:

.. code-block:: sh

   gnatcov coverage \
     --checkpoint=text_io.ckpt \
     --checkpoint=gnat_io.ckpt \
     --level=stmt --annotate=xcov --output-dir=xcov

As for the first method, this yields the expected coverage report, mentionning
the two alternative implementations of ``Logging``:

.. code-block:: sh

   cat xcov/logging.ads.xcov
   /tmp/foo/logging.ads:
   no code
   no code
   Coverage level: stmt
      1 .:    package Logging is
      2 .:       procedure Warn (Message : String);
      3 .:       procedure Error (Message : String);
      4 .:    end Logging;

   cat xcov/logging__text_io.adb.xcov
   /tmp/foo/logging__text_io.adb:
   75% of 4 lines covered
   75% statement coverage (3 out of 4)

   Coverage level: stmt
      1 .:    with Ada.Text_IO; use Ada.Text_IO;
      2 .:    package body Logging is
      3 .:
      4 .:       procedure Write (Prefix, Message : String) is
      5 .:       begin
      6 +:          Put (Prefix);
      7 +:          Put_Line (Message);
      8 .:       end Write;
      9 .:
     10 .:       procedure Warn (Message : String) is
     11 .:       begin
     12 +:          Write ("warning: ", Message);
     13 .:       end Warn;
     14 .:
     15 .:       procedure Error (Message : String) is
     16 .:       begin
     17 -:          Write ("error: ", Message);
     18 .:       end Error;
     19 .:    end Logging;
   cat xcov/logging__gnat_io.adb.xcov
   /tmp/foo/logging__gnat_io.adb:
   80% of 5 lines covered
   80% statement coverage (4 out of 5)

   Coverage level: stmt
      1 .:    with GNAT.IO; use GNAT.IO;
      2 .:    package body Logging is
      3 .:
      4 .:       procedure Write (Prefix, Message : String) is
      5 .:       begin
      6 +:          Put (Prefix);
      7 +:          Put (Message);
      8 +:          New_Line;
      9 .:       end Write;
     10 .:
     11 .:       procedure Warn (Message : String) is
     12 .:       begin
     13 +:          Write ("warning: ", Message);
     14 .:       end Warn;
     15 .:
     16 .:       procedure Error (Message : String) is
     17 .:       begin
     18 -:          Write ("error: ", Message);
     19 .:       end Error;
     20 .:    end Logging;
