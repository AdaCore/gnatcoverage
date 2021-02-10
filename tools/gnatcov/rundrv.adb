------------------------------------------------------------------------------
--                                                                          --
--                               GNATcoverage                               --
--                                                                          --
--                     Copyright (C) 2009-2021, AdaCore                     --
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

with Ada.Command_Line; use Ada.Command_Line;
with Ada.Directories;  use Ada.Directories;
with Ada.Environment_Variables;
with Ada.Text_IO;      use Ada.Text_IO;
with Ada.Unchecked_Conversion;

with Interfaces;

with GNAT.OS_Lib;

with Binary_Files;
with Execs_Dbase;
with Outputs;
with Qemu_Traces;
with Rundrv.Config; use Rundrv.Config;
with Switches;      use Switches;
with Traces_Elf;
with Traces_Files;  use Traces_Files;

package body Rundrv is

   package Env renames Ada.Environment_Variables;

   Native_Warning : constant String :=
      "Support for coverage of non-instrumented native programs is deprecated"
      & " and will disappear after GNATcoverage 21 releases. You are"
      & " encouraged to migrate to instrumentation-based coverage: you can"
      & " read more about it in our documentation:"
      & " <http://docs.adacore.com/gnatcoverage-docs/html/gnatcov.html>";
   --  Warning to emit when running native programs

   Native_Warning_Envvar : constant String := "GNATCOV_NO_NATIVE_WARNING";
   --  Name of the environment variable to define in order to disable this
   --  warning.

   --  Variables set by the command line.

   Exec_Error : exception;
   --  Raised when subprogram execution failed. The error message shall be
   --  generated before raising the exception.

   procedure Error (Msg : String);
   --  Display the message on the error output and set exit status

   procedure Run_Command (Command : Command_Type);
   --  Spawn a command

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
        (Kernel   => Kernel,
         Histmap  => Histmap,
         Eargs    => Eargs,
         others   => <>);
      Run_Cmd : Command_Access;

   --  Start of processing for Driver

   begin

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

         Run_Cmd := Lookup_Driver (Context);

         if Run_Cmd = null then
            Error ("No builtin or GNATemulator execution driver found for"
                   & " target: " & Context.Target_Family.all);
            return;

         elsif Run_Cmd.Native
               and then Env.Value (Native_Warning_Envvar, "") = ""
         then
            Outputs.Warn (Native_Warning);
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

      --  Some setup operations work out of mere side effect of macro
      --  expansions, e.g.  setting environment variables. The expansion is
      --  required, but there's no real command to execute afterwards

      Run_Command (Run_Cmd.all);
      Free (Run_Cmd);

   exception
      when Exec_Error =>
         Set_Exit_Status (Failure);
   end Driver;

   -----------
   -- Error --
   -----------

   procedure Error (Msg : String) is
   begin
      Put_Line (Standard_Error, Msg);
      Set_Exit_Status (Failure);
   end Error;

   -----------------
   -- Run_Command --
   -----------------

   procedure Run_Command (Command : Command_Type) is
      use String_Maps;
      Success : Boolean;
      Prg     : String_Access;
      Args    : String_List (1 .. Natural (Command.Arguments.Length));

      Cmd : constant String := +Command.Command;
   begin

      --  Honor a possible empty command text, meaning no actual
      --  command to run.

      if Cmd'Length = 0 then
         return;
      end if;

      --  Find executable

      Prg := GNAT.OS_Lib.Locate_Exec_On_Path (Cmd);
      if Prg = null then
         Error ("gnatcov run: cannot find " & Cmd & " on your path");
         raise Exec_Error;
      end if;

      --  Instantiate the argument list

      declare
         I : Positive := 1;
      begin
         for S of Command.Arguments loop
            Args (I) := new String'(+S);
            I := I + 1;
         end loop;
      end;

      --  Run

      for Env_Var in Command.Environment.Iterate loop
         if Verbose then
            Put_Line ("env: " & (+Key (Env_Var))
                      & "=" & (+Element (Env_Var)));
         end if;
         GNAT.OS_Lib.Setenv (+Key (Env_Var), +Element (Env_Var));
      end loop;

      if Verbose then
         Put ("exec: ");
         Put (Prg.all);
         for S of Command.Arguments loop
            Put (' ');
            Put (+S);
         end loop;
         New_Line;
      end if;

      GNAT.OS_Lib.Spawn (Prg.all, Args, Success);
      if not Success then
         if Verbose then
            Error ("gnatcov run failed");
         end if;
         raise Exec_Error;
      end if;

      if Verbose then
         Put_Line (Cmd & " finished");
      end if;
   end Run_Command;

end Rundrv;
