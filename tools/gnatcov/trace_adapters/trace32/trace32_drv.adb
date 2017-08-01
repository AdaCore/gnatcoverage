------------------------------------------------------------------------------
--                                                                          --
--                               GNATcoverage                               --
--                                                                          --
--                     Copyright (C) 2012-2017, AdaCore                     --
--                                                                          --
-- GNATcoverage is free software; you can redistribute it and/or modify it  --
-- under terms of the GNU General Public License as published by the  Free  --
-- Software  Foundation;  either version 3,  or (at your option) any later  --
-- version. This software is distributed in the hope that it will be useful --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for  more details.  You should have  received  a copy of the GNU --
-- General  Public  License  distributed  with  this  software;   see  file --
-- COPYING3.  If not, go to http://www.gnu.org/licenses for a complete copy --
-- of the license.                                                          --
------------------------------------------------------------------------------

with Ada.Command_Line;   use Ada.Command_Line;
with Text_IO;            use Text_IO;

with GNAT.OS_Lib;        use GNAT.OS_Lib;

with Outputs;

with Trace32.Conversion;

--  This program  - trace32_drv - is called by 'gnatcov run' and by
--  'gnatcov convert' when the target specified to gnatcov is one that
--  indicates that the program should be run (in the case of "'gnatcov
--  run') or was run ('gnatcov convert') through an Trace32 probe, on a
--  target board connected to that probe.
--
--  When called by 'gnatcov convert', the target executable has already
--  been executed on the processor, and generated a Branch Flow Trace
--  data file. Trace32_drv runs the program that converts the trace
--  file into the GNATCoverage format, for use by 'gnatcov coverage'.
--
--  When called by 'gnatcov run', trace32_drv controls the loading and
--  execution of the program on the target hardware to generate the
--  branch trace file, and then runs the translation step. To accomplish
--  the execution on the target, trace32_drv:
--   - Runs a python program which directs Trace32 to run the executable
--     program on the board and generate program trace data in a file.
--
--   - Runs a translation program which converts the trace from Trace32
--     into the format used by gnatcov.
--
--  When called for 'gnatcov run', 3 arguments are passed. When
--  called for 'gnatcov convert' 7 arguments are passed with the
--  first 3 being the same as for 'run'. The first 2 designate the
--  target processor and the executable run on the target. The
--  third argument is either the name of the trace file initialized by
--  gnatcov and appended to by the trace conversion program, or an argument
--  of the form 'histmap=HH,TT', where TT is the name of the trace file,
--  and HH is the name of a history file which is passed to the trace
--  conversion program to tell it to output 'history' traces and identifying
--  addresses for which history information is needed.
--
--  Their is some error checking done by this program, but
--  since it is intended to be called from gnatcov (no fat
--  fingers), many things are not checked.
--
--  This program should reside in ../libexec/gnatcoverage, relative
--  to the directory containing the gnatcov program. It expects
--  that other programs that it calls (the python program, and
--  the trace conversion program) to reside in the same directory.
--  It expcts to find the Trace32 configuration files in
--  ../../share/gnatcoverage (relative to trave32_drv itself).

procedure Trace32_Drv is

   type Drv_Mode is (Run, Convert);
   Mode : Drv_Mode;

   S_Idx1, S_Idx2 : Positive;

   Processor       : String_Access with Unreferenced;
   --  The processor name will be useful for the run mode that is not yet
   --  implemented.

   Branchflow_Path : String_Access;
   Executable_Path : String_Access;
   Histfile_Path   : String_Access;
   Tracefile_Path  : String_Access;
   --  Pathnames for the executable run on the board,
   --  the history file and the trace file.

begin
   Outputs.Warn ("=== Trace32 trace converter ===");

   if Argument_Count = 3 then
      Mode := Run;
   elsif Argument_Count = 4 then
      Mode := Convert;
   else
      Put_Line (Standard_Error, "Wrong # or args to trace32_drv.");
      OS_Exit (1);
   end if;

   Executable_Path := new String'(Normalize_Pathname (Argument (2)));
   if Mode = Convert then
      Branchflow_Path := new String'(Normalize_Pathname (Argument (4)));
   end if;
   Processor := new String'(Argument (1));

   --  Handle the casses where the tracefile argument is either the
   --  name of the trace file created by gnatcov (with some information
   --  records, and to be appended with the trace records), or of the
   --  form histmap=H,T with H being the name of a history map file
   --  and T is the name of the trace file.
   --
   --  Here we create the path names of the files, or "" for the history
   --  map file in the first case above.

   S_Idx1 := Argument (3)'First;
   if Argument (3)'Length > 8 and then
     Argument (3) (S_Idx1 .. S_Idx1 + 7) = "histmap="
   then
      S_Idx1 := S_Idx1 + 8;
      S_Idx2 := S_Idx1;
      loop
         exit when S_Idx2 = Argument (3)'Last
           or else Argument (3) (S_Idx2) = ',';
         S_Idx2 := S_Idx2 + 1;
      end loop;
      if S_Idx2 = Argument (3)'Last then
         Put_Line (Standard_Error, "Missing tracefile arg to trace32_drv.");
         OS_Exit (1);
      end if;
      Histfile_Path := new String'
        (Normalize_Pathname (Argument (3) (S_Idx1 .. S_Idx2 - 1)));
      Tracefile_Path := new String'
        (Normalize_Pathname (Argument (3) (S_Idx2 + 1 .. Argument (3)'Last)));
   else
      Histfile_Path := new String'("");
      Tracefile_Path := new String'(Argument (3));
   end if;

   if Mode = Run then
      Outputs.Fatal_Error ("Run mode not supported yet");
      --  Trace32.API.Load;
      --  Trace32.API.Connect ("localhost", "20000", "1024");
      --  if Processor.all = "stm32f7" then
      --     Trace32.API.STM32F7_Setup;
      --  end if;
      --  Trace32.API.Load_Executable (Executable_Path.all);
      --  Trace32.API.Set_Breakpoint ("__gnat_last_chance_handler");
      --  Trace32.API.Run_Until ("_exit", 500);
      --  Trace32.API.Export_Branchflow_Trace (Tracefile_Path.all &
      --                                         ".branchflow");
   else
      Outputs.Warn ("Starting to convert...");

      Trace32.Conversion.Convert_Branchflow_Trace
        (Elf_File          => Executable_Path.all,
         Branchflow_File   => Branchflow_Path.all,
         Qemu_Trace_File   => Tracefile_Path.all,
         Decision_Map_File => Histfile_Path.all);
   end if;
   OS_Exit (0);
end Trace32_Drv;
