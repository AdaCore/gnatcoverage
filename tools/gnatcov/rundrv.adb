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
with Ada.Unchecked_Deallocation;

with Ada.Text_IO;      use Ada.Text_IO;
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Directories;  use Ada.Directories;

with Interfaces;

with GNAT.OS_Lib;
with GNAT.Regpat;  use GNAT.Regpat;

with Execs_Dbase;
with Qemu_Traces;
with Switches;     use Switches;
with Traces_Elf;
with Traces_Files; use Traces_Files;

with Rundrv.Config;   use Rundrv.Config;
with Rundrv.Expander; use Rundrv.Expander;
with Rundrv.State;    use Rundrv.State;

package body Rundrv is

   Target_Default : constant String_Access :=
                      new String'(Standard'Target_Name);

   --  Variables set by the command line.

   Exec_Error : exception;
   --  Raised when subprogram execution failed. The error message shall be
   --  generated before raising the exception.

   procedure Error (Msg : String);
   --  Display the message on the error output and set exit status

   procedure Run_Command (Command : String_Access; Options : String_List);
   --  Spawn command with options

   type Driver_Target_Access is access constant Driver_Target;

   function Driver_Control_For
     (Target, Kernel : String_Access) return Driver_Target_Access;
   --  Return an access to the Driver_Target control block to use for
   --  TARGET/KERNEL. This will be a <target>-gnatemu block if GNATemulator is
   --  available on PATH, or a low-level emulator block from our static
   --  configuration table otherwise. Return null if we can't figure out any
   --  sensible control block for the provided target name. KERNEL is the
   --  command line --kernel argument, if any. TARGET is the command line
   --  --target value, which may feature an optional board specification
   --  (e.g. --target=powerpc-elf,prep).

   --  The computation is split across the two helpers below.  TARGET_FAMILY
   --  and TARGET_BOARD are set to the base target and board extension of the
   --  original TARGET input.

   function Gnatemu_Driver_Control_For
     (Target_Family : String_Access;
      Target_Board  : String_Access;
      Kernel        : String_Access) return Driver_Target_Access;
   --  Implement the GNATemulator case of Driver_Control_For

   function Internal_Driver_Control_For
     (Target_Family : String_Access;
      Target_Board  : String_Access;
      Kernel        : String_Access) return Driver_Target_Access;
   --  Implement the internal tables case of Driver_Control_For

   -----------------
   -- Real_Target --
   -----------------

   function Real_Target (Target : String_Access) return String_Access
   is
   begin
      return (if Target = null
              then Target_Default
              else Target);
   end Real_Target;

   --------------------------------
   -- Gnatemu_Driver_Control_For --
   --------------------------------

   function Gnatemu_Driver_Control_For
     (Target_Family : String_Access;
      Target_Board  : String_Access;
      Kernel        : String_Access) return Driver_Target_Access
   is
      Gnatemu : constant String_Access :=
        GNAT.OS_Lib.Locate_Exec_On_Path (Target_Family.all & "-gnatemu");

      Common_Options, Kernel_Options, Board_Options : String_List_Access;
      P_Options, X_Options : String_List_Access;

      function X_Switches return String_List_Access;
      --  Compute a string list of -X switches to pass to gnatemu from the set
      --  of -X switches we have received on our own command line.

      function X_Switches return String_List_Access is
         use Key_Element_Maps;

         Switches : constant String_List_Access :=
           new String_List (1 .. Integer (Length (S_Variables)));
         Switch_Index : Natural;
      begin
         Switch_Index := Switches'First;
         for Scv_C in S_Variables.Iterate loop
            Switches (Switch_Index) :=
              new String'("-X" & Key (Scv_C) & "=" & Element (Scv_C));
            Switch_Index := Switch_Index + 1;
         end loop;
         return Switches;
      end X_Switches;

   begin

      if Gnatemu = null then
         return null;
      end if;

      --  Compute the subsets of options we need to pass.  As common options,
      --  we always need to pass the executable name to Gnatemu (last), and
      --  request the production of traces straight to the underlying emulator
      --  in addition to our own -eargs. Then we have the possible --kernel
      --  and --board extensions, and the project file related options.

      Common_Options := new String_List'
        ((new String'("--eargs"),
          new String'("-exec-trace"),
          new String'("%trace"),
          new String'("%eargs"),
          new String'("--eargs-end"),
          new String'("%exe")));

      if Kernel = null then
         Kernel_Options := new String_List'(1 .. 0 => <>);
      else
         Kernel_Options := new String_List'
           (1 => new String'("--kernel=" & Kernel.all));
      end if;

      if Target_Board = null then
         Board_Options := new String_List'(1 .. 0 => <>);
      else
         Board_Options := new String_List'
           (1 => new String'("--board=" & Target_Board.all));
      end if;

      if Root_Project = null then
         P_Options := new String_List'(1 .. 0 => <>);
         X_Options := new String_List'(1 .. 0 => <>);
      else
         P_Options := new String_List'
           ((new String'("-P"), new String'(Root_Project.all)));
         X_Options := X_Switches;
      end if;

      --  Now construct the global set of options and the control
      --  record to return.

      declare
         N_Options : constant Natural :=
           Common_Options'Length
           + Kernel_Options'Length
           + Board_Options'Length
           + P_Options'Length
           + X_Options'Length;

         Gnatemu_Options : constant String_List_Access :=
           new String_List (1 .. N_Options);

         --  To help accumulate options subsets within Gnatemu_Options:

         Next_Gnatemu_Option : Natural := Gnatemu_Options'First;
         procedure Add_And_Release (Options : in out String_List_Access);

         --  Once the options are latched in Gnatemu_Options, we
         --  don't need the subset containers any more.

         procedure Free_Array is new Ada.Unchecked_Deallocation
           (Object => String_List, Name => String_List_Access);

         procedure Add_And_Release (Options : in out String_List_Access) is
         begin
            for I in Options'Range loop
               Gnatemu_Options (Next_Gnatemu_Option) := Options (I);
               Next_Gnatemu_Option := Next_Gnatemu_Option + 1;
            end loop;
            Free_Array (Options);
         end Add_And_Release;

      begin
         Add_And_Release (Kernel_Options);
         Add_And_Release (Board_Options);
         Add_And_Release (P_Options);
         Add_And_Release (X_Options);
         Add_And_Release (Common_Options);

         return new Driver_Target'
           (Target        => Target_Family,
            Setup_Command => null,
            Setup_Options => null,
            Run_Command   => Gnatemu,
            Run_Options   => Gnatemu_Options);
      end;
   end Gnatemu_Driver_Control_For;

   ---------------------------------
   -- Internal_Driver_Control_For --
   ---------------------------------

   function Internal_Driver_Control_For
     (Target_Family : String_Access;
      Target_Board  : String_Access;
      Kernel        : String_Access) return Driver_Target_Access
   is
      pragma Unreferenced (Target_Board);
      pragma Unreferenced (Kernel);

      Resolved_Target : String_Access := Target_Family;
   begin
      --  Resolve against our target board Aliases table first,
      --  then seek a matching Driver entry.

      for I in Aliases'Range loop
         if Resolved_Target.all = Aliases (I).Alias.all then
            Resolved_Target := Aliases (I).Target;
         end if;
      end loop;

      for I in Drivers'Range loop
         if Match (Expression => Drivers (I).Target.all,
                   Data => Resolved_Target.all)
         then
            return Drivers (I)'Access;
         end if;
      end loop;

      return null;
   end Internal_Driver_Control_For;

   ------------------------
   -- Driver_Control_For --
   ------------------------

   function Driver_Control_For
     (Target, Kernel : String_Access) return Driver_Target_Access
   is
      Control : Driver_Target_Access;

      --  We need to extract a possible <board> extension from the target
      --  specification first. The input value is expected to be like
      --  <target-family>,<target-board> where the ",<target-board>" extension
      --  is optional. We use a simple regular expression matcher for this.

      Target_Family, Target_Board : String_Access := null;

      Matches : Match_Array (1 .. 2);
      Family_Part : Match_Location renames Matches (1);
      Board_Part  : Match_Location renames Matches (2);
   begin
      Match (Expression => "^([^,]*)(,.*)?", Data => Target.all,
             Matches => Matches);

      Target_Family := new String'
        (Target (Family_Part.First .. Family_Part.Last));

      if Board_Part /= No_Match then
         Target_Board := new String'
           (Target (Board_Part.First + 1 .. Board_Part.Last));
      end if;

      --  If we have GNATemulator for Target on PATH, use that.  --target
      --  values provided by users are expected to match the GNATemulator
      --  target names in such cases.

      --  Otherwise, see if we have a bare internal emulator entry for Target.
      --  The target name might be a generic alias for a low-level board name
      --  in this case.

      Control := Gnatemu_Driver_Control_For (Target_Family,
                                             Target_Board,
                                             Kernel);
      if Control /= null then
         return Control;
      end if;

      return Internal_Driver_Control_For (Target_Family,
                                          Target_Board,
                                          Kernel);
   end Driver_Control_For;

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
      Control : Driver_Target_Access;
      --  The corresponding Driver_Target control block

   --  Start of processing for Driver

   begin
      --  Setup our basic internal control parameters

      Control :=
        Driver_Control_For (Target => Real_Target (Target), Kernel => Kernel);

      if Control = null then
         Error ("unknown target " & Real_Target (Target).all
                & " (use --help to get target list)");
         --  ??? xcov run --help should give the target list
         return;
      end if;

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
         Open_Exec (Exe_File, 0, Real_Target (Target), Exec);

         --  Setup global state: we had to wait for opening the executable file
         --  since the trace file path depends on the precise executable file
         --  name.

         Executable := new String'(Get_Filename (Exec.all));

         if GNAT.Strings."/=" (Output, null) then
            Trace_Output := Output;
         else
            Trace_Output :=
               new String'(Simple_Name (Executable.all & ".trace"));
         end if;

         Histmap_Filename := Histmap;

         --  And now create the trace file itself.

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

         Append_Info
           (Trace_File,
            Exec_File_Size,
            Long_Integer'Image (Get_Size (Exec.all)));
         Append_Info
           (Trace_File,
            Exec_File_Time_Stamp,
            Time_Stamp_Image (Get_Time_Stamp (Exec.all)));
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
      P ("run [OPTIONS] [EXE] [-eargs [EXE] EARGS...]");
      P ("  -t | --target=TARGET[,BOARD] Select the execution target");
      P ("                               for programs built with a cross");
      P ("                               toolchain.");
      P (Indent
           & "     targets: A prefix to a version of gnatemu on PATH, "
           & "or one of");

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
      P ("  -eargs EARGS                 For emulated cross targets, pass");
      P ("                               EARGS to the low-level emulator.");
      P ("                               For native configurations, pass");
      P ("                               EARGS as command line arguments to");
      P ("                               the executable program.");
      P ("                               The first EARG is picked as the");
      P ("                               EXEcutable program to run if it was");
      P ("                               not provided explicitly otherwise.");
      P ("  --kernel=FILE                Specify which kernel to use");
      P ("  --level=CRIT                 Assume CRIT as the strictest future");
      P ("                               analysis criterion.");
      P ("  -P, --projects, --units,     Designate units of interest when");
      P ("  --recursive, --scos          mcdc coverage analysis is intended");
      P ("                               later on. See ""coverage"" options.");
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
