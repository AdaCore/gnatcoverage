------------------------------------------------------------------------------
--                                                                          --
--                               GNATcoverage                               --
--                                                                          --
--                     Copyright (C) 2009-2013, AdaCore                     --
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

with Ada.Unchecked_Conversion;
with Ada.Text_IO;      use Ada.Text_IO;
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Directories;  use Ada.Directories;

with Interfaces;

with GNAT.OS_Lib;

with Qemu_Traces;
with Switches;     use Switches;
with Traces_Files; use Traces_Files;

with Rundrv.Config;   use Rundrv.Config;
with Rundrv.Expander; use Rundrv.Expander;
with Rundrv.State;    use Rundrv.State;

package body Rundrv is

   Target_Default : constant String_Access
     := new String'(Standard'Target_Name);

   --  Variables set by the command line.

   Exec_Error : exception;
   --  Raised when subprogram execution failed. The error message shall be
   --  generated before raising the exception.

   procedure Error (Msg : String);
   --  Display the message on the error output and set exit status

   procedure Run_Command (Command : String_Access; Options : String_List);
   --  Spawn command with options

   ------------
   -- Driver --
   ------------

   procedure Driver
     (Exe_File : String;
      Target   : String_Access;
      Tag      : String_Access;
      Output   : String_Access;
      Histmap  : String_Access;
      Kernel   : String_Access;
      Eargs    : String_List_Access)
   is
      type Driver_Target_Access is access constant Driver_Target;

      function Driver_Control_For
        (Target : String_Access) return Driver_Target_Access;
      --  Return an access to the Driver_Target control block to use for
      --  TARGET. This will be a <target>-gnatemu block if GNATemulator is
      --  available on PATH, or a low-level emulator block from our static
      --  configuration table otherwise. Return null if we can't figure out
      --  any sensible control block for the provided target name.

      ------------------------
      -- Driver_Control_For --
      ------------------------

      function Driver_Control_For
        (Target : String_Access) return Driver_Target_Access
      is
      begin

         --  If we have GNATemulator for Target on PATH, use that. --target
         --  values provided by users are expected to match the GNATemulator
         --  target names in such cases, so there's no point looking into
         --  the Aliases entries here.

         declare
            Gnatemu : constant String_Access
              := GNAT.OS_Lib.Locate_Exec_On_Path (Target.all & "-gnatemu");
            List : String_List_Access;
         begin

            if Gnatemu /= null then

               --  We just need to pass the executable name to Gnatemu (last),
               --  and request the production of traces straight to the
               --  underlying emulator in addition to our own -eargs.

               if Kernel = null then
                  List := new String_List (1 .. 6);
                  List (6) := new String'("%exe");
               else
                  List := new String_List (1 .. 7);
                  List (6) := new String'("--kernel=" & Kernel.all);
                  List (7) := new String'("%exe");
               end if;
               List (1 .. 5) := (new String'("--eargs"),
                                 new String'("-exec-trace"),
                                 new String'("%trace"),
                                 new String'("%eargs"),
                                 new String'("--eargs-end"));
               return new Driver_Target'
                 (Target => Target,
                  Setup_Command => null,
                  Setup_Options => null,
                  Run_Command => Gnatemu,
                  Run_Options => List);
            end if;
         end;

         --  Otherwise, see if we have a bare emulator entry for Target. The
         --  target name might be a generic alias for a low-level board name
         --  in this case.

         declare
            Resolved_Target : String_Access := Target;
         begin

            --  Resolve against our target board Aliases table first,
            --  then seek a matching Driver entry.

            for I in Aliases'Range loop
               if Resolved_Target.all = Aliases (I).Alias.all then
                  Resolved_Target := Aliases (I).Target;
               end if;
            end loop;

            for I in Drivers'Range loop
               if Drivers (I).Target.all = Resolved_Target.all then
                  return Drivers (I)'Access;
               end if;
            end loop;
         end;

         --  Nothing we can do if we haven't found anything at this point

         return null;

      end Driver_Control_For;

      Real_Target : not null String_Access := Target_Default;
      --  What we should be using as the target name, possibly not
      --  provided on entry.

      Control : Driver_Target_Access;
      --  The corresponding Driver_Target control block

   --  Start of processing for Driver

   begin
      --  Setup our basic internal control parameters

      if Target /= null then
         Real_Target := Target;
      end if;

      Control := Driver_Control_For (Real_Target);

      if Control = null then
         Error ("unknown target " & Real_Target.all
                & " (use --help to get target list)");
         --  ??? xcov run --help should give the target list
         return;
      end if;

      --  Setup global state

      if Output /= null then
         Trace_Output := Output;
      else
         Trace_Output := new String'(Simple_Name (Exe_File & ".trace"));
      end if;

      Histmap_Filename := Histmap;

      Executable := new String'(Exe_File);

      --  Create the trace file

      declare
         use GNAT.OS_Lib;
         use Qemu_Traces;
         use Interfaces;
         Trace_File : Trace_File_Type;
         Date_Info  : Trace_Info_Date;
         Date       : constant OS_Time := Current_Time;
         subtype String_8 is String (1 .. 8);
         function Date_Info_To_Str is new Ada.Unchecked_Conversion
           (Trace_Info_Date, String_8);
      begin
         Create_Trace_File (Info, Trace_File);
         Date_Info := Trace_Info_Date'(Year  => Unsigned_16 (GM_Year (Date)),
                                       Month => Unsigned_8 (GM_Month (Date)),
                                       Day   => Unsigned_8 (GM_Day (Date)),
                                       Hour  => Unsigned_8 (GM_Hour (Date)),
                                       Min   => Unsigned_8 (GM_Minute (Date)),
                                       Sec   => Unsigned_8 (GM_Second (Date)),
                                       Pad   => 0);
         Append_Info (Trace_File, Date_Time, Date_Info_To_Str (Date_Info));
         Append_Info (Trace_File, Exec_File_Name, Exe_File);

         if GNAT.Strings."/=" (Tag, null) then
            Append_Info (Trace_File, User_Data, Tag.all);
         end if;
         if GNAT.Strings."/=" (Kernel, null) then
            Append_Info (Trace_File, Kernel_File_Name, Kernel.all);
         end if;

         Write_Trace_File (Trace_Output.all, Trace_File);
         Free (Trace_File);
      end;

      --  Execute whatever we need to prepare the execution

      --  Some setup operations work out of mere side effect of macro
      --  expansions, e.g.  setting environment variables. The expansion is
      --  required, but there's no real command to execute afterwards

      declare
         Expanded_Setup_Command : constant String_Access
           := Expand_Command (Control.Setup_Command);
      begin
         if Expanded_Setup_Command /= null then
            Run_Command (Expanded_Setup_Command, Control.Setup_Options.all);
         end if;
      end;

      --  Now proceed with the execution per se. Same logic.

      declare
         Expanded_Run_Command : constant String_Access
           := Expand_Command (Control.Run_Command);
      begin
         if Expanded_Run_Command /= null then

            Run_Command
              (Expanded_Run_Command,
               Expand_Arguments (Control.Run_Options,
                                 Eargs => Eargs));

            if Verbose then
               Put (Control.Run_Command.all & " finished");
            end if;
         end if;
      end;

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

   ----------
   -- Help --
   ----------

   procedure Help (Indent : String := "") is

      procedure P (Str : String);
      --  Put_Line with proper indentation

      -------
      -- P --
      -------

      procedure P (Str : String) is
      begin
         Put_Line (Indent & Str);
      end P;

      Driver_Idx : Natural;
      Filled     : Positive;

   --  Start of processing for Help

   begin
      P ("run [OPTIONS] [EXE] [-eargs EARGS...]");
      P ("  -t TARGET  --target=TARGET   Set the execution target");
      P (Indent & "    targets: A prefix to a version of gnatemu on PATH, or");

      Driver_Idx := Drivers'First;
      loop
         exit when Driver_Idx > Drivers'Last;
         Put  (Indent & "      " & Drivers (Driver_Idx).Target.all);
         Filled := Indent'Length + 6 + Drivers (Driver_Idx).Target'Length;
         loop
            Driver_Idx := Driver_Idx + 1;
            if Driver_Idx > Drivers'Last then
               New_Line;
               exit;
            end if;
            Filled := Filled + 1 + Drivers (Driver_Idx).Target'Length;
            if Filled >= 80 then
               New_Line;
               exit;
            end if;
            Put (' ' & Drivers (Driver_Idx).Target.all);
         end loop;
      end loop;
      --  Perhaps there is a library function suitable for doing
      --  the line filling that is done by hand here. ???

      P ("  -v --verbose                 Be verbose");
      P ("  -T TAG  --tag=TAG            Put TAG in tracefile");
      P ("  -o FILE  --output=FILE       Write traces to FILE");
      P ("  -eargs EARGS                 " &
           "Pass EARGS to the low-level emulator.");
      P ("                               " &
           "First earg is picked as the EXE program");
      P ("                               " &
           "to run if not provided explicitly.");
      P ("  --kernel=FILE                Specify which kernel to use");
   end Help;

   -----------------
   -- Run_Command --
   -----------------

   procedure Run_Command
     (Command : String_Access; Options : String_List)
   is
      Success : Boolean;
      Prg     : String_Access;
   begin
      --  Find executable

      Prg := GNAT.OS_Lib.Locate_Exec_On_Path (Command.all);
      if Prg = null then
         Error ("gnatcov run: cannot find "
                  & Command.all
                  & " on your path");
         raise Exec_Error;
      end if;

      if Verbose then
         Put ("exec: ");
         Put (Prg.all);
         for I in Options'Range loop
            Put (' ');
            Put (Options (I).all);
         end loop;
         New_Line;
      end if;

      --  Run

      GNAT.OS_Lib.Spawn (Prg.all, Options, Success);
      if not Success then
         if Verbose then
            Error ("gnatcov run failed");
         end if;
         raise Exec_Error;
      end if;
   end Run_Command;

end Rundrv;
