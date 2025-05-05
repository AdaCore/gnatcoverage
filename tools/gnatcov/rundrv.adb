------------------------------------------------------------------------------
--                                                                          --
--                               GNATcoverage                               --
--                                                                          --
--                     Copyright (C) 2009-2024, AdaCore                     --
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

with Ada.Directories;  use Ada.Directories;
with Ada.Environment_Variables;
with Ada.Unchecked_Conversion;

with Interfaces;

with GNAT.OS_Lib;

with Binary_Files;
with Execs_Dbase;
with Outputs;
with Qemu_Traces;
with Rundrv.Config; use Rundrv.Config;
with Subprocesses;  use Subprocesses;
with Traces_Elf;
with Traces_Files;  use Traces_Files;

package body Rundrv is

   package Env renames Ada.Environment_Variables;

   Warning : constant String :=
      "Support for coverage of non-instrumented programs is deprecated"
      & " and will disappear after GNATcoverage 26 releases. You are"
      & " encouraged to migrate to instrumentation-based coverage: you can"
      & " read more about it in our documentation:"
      & " <https://docs.adacore.com/live/wave/gnatdas/html/gnatdas_ug/gnatcov"
      & "/src_traces.html>";
   --  Warning to emit when using binary traces

   Warning_Envvar : constant String := "GNATCOV_NO_BINARY_TRACES_WARNING";
   --  Name of the environment variable to define in order to disable this
   --  warning.

   ------------
   -- Driver --
   ------------

   procedure Driver
     (Exe_File      : String;
      Target_Family : String_Access;
      Target_Board  : String_Access;
      Tag           : String_Access;
      Output        : String_Access;
      Histmap       : String_Access;
      Kernel        : String_Access;
      Eargs         : String_List_Access;
      SO_Set        : SO_Set_Type)
   is
      pragma Unreferenced (SO_Set);
      --  TODO??? Handle shared objects

      Context : Context_Type :=
        (Kernel  => Kernel,
         Histmap => Histmap,
         Eargs   => Eargs,
         others  => <>);
      Found   : Boolean;
      Run_Cmd : Command_Type;
      Native  : Boolean;
      Dummy   : Boolean;
   begin

      --  Create the first part of the execution trace file: Info header + the
      --  associated Trace Info Entries.

      declare
         use GNAT.OS_Lib;

         use Interfaces;

         use Execs_Dbase;
         use Qemu_Traces;
         use Traces_Elf;

         Exec       : Exe_File_Acc;
         Trace_File : Trace_File_Type;
         Date_Info  : Trace_Info_Date;
         Date       : constant OS_Time := Current_Time;
         subtype String_8 is String (1 .. 8);
         function Date_Info_To_Str is new Ada.Unchecked_Conversion
           (Trace_Info_Date, String_8);

      begin
         Open_Exec (Exe_File, 0, Exec);

         Context.Exe_File := new String'(Get_Filename (Exec.all));
         Context.Trace_File :=
           (if GNAT.Strings."/=" (Output, null)
            then Output
            else new String'(Simple_Name (Context.Exe_File.all & ".trace")));
         Context.Target_Family := Target_Family;
         Context.Target_Board := Target_Board;

         Lookup_Driver (Context, Found, Run_Cmd, Native);

         if not Found then
            Outputs.Error
              ("No builtin or GNATemulator execution driver found for"
               & " target: " & Context.Target_Family.all);
            return;
         end if;

         --  And now create the trace file itself.

         Create_Trace_File (Context.Trace_File.all, Info, Trace_File);
         Date_Info := Trace_Info_Date'(Year  => Unsigned_16 (GM_Year (Date)),
                                       Month => Unsigned_8 (GM_Month (Date)),
                                       Day   => Unsigned_8 (GM_Day (Date)),
                                       Hour  => Unsigned_8 (GM_Hour (Date)),
                                       Min   => Unsigned_8 (GM_Minute (Date)),
                                       Sec   => Unsigned_8 (GM_Second (Date)),
                                       Pad   => 0);
         Append_Info (Trace_File, Date_Time, Date_Info_To_Str (Date_Info));
         Append_Info (Trace_File, Exec_File_Name, Context.Exe_File.all);

         Append_Info
           (Trace_File,
            Exec_File_Size,
            Long_Integer'Image (Get_Size (Exec.all)));
         Append_Info
           (Trace_File,
            Exec_File_Time_Stamp,
            Binary_Files.Time_Stamp_Image (Get_Time_Stamp (Exec.all)));
         Append_Info
           (Trace_File,
            Exec_File_CRC32,
            Unsigned_32'Image (Get_CRC32 (Exec.all)));

         if GNAT.Strings."/=" (Tag, null) then
            Append_Info (Trace_File, User_Data, Tag.all);
         end if;
         if GNAT.Strings."/=" (Kernel, null) then
            Append_Info (Trace_File, Kernel_File_Name, Kernel.all);
         end if;

         Write_Trace_File (Trace_File);
         Free (Trace_File);
      end;

      --  Run the instrumented execution environment for the user program,
      --  which will append to the trace file its second part: the
      --  Flat|History header + the associated trace entries.
      --
      --  Note that as far as "gnatcov run" is concerned, a non-zero status
      --  code from this subprocess is not an error for trace production, as it
      --  may reflect a non-zero status code from the user program.
      --
      --  Also note that for the special "prepare*" targets, we do not have any
      --  command to run, as we just need to create the trace file header: do
      --  not call Run_Command in this case.

      if Run_Cmd.Command /= "" then
         Dummy := Run_Command (Run_Cmd, "gnatcov run", Ignore_Error => True);
      end if;
   end Driver;

   ------------------------------
   -- Emit_Deprecation_Warning --
   ------------------------------

   procedure Emit_Deprecation_Warning is
   begin
      if Env.Value (Warning_Envvar, "") = "" then
         Outputs.Warn (Warning);
      end if;
   end Emit_Deprecation_Warning;

end Rundrv;
