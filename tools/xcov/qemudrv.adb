------------------------------------------------------------------------------
--                                                                          --
--                              Couverture                                  --
--                                                                          --
--                        Copyright (C) 2009, AdaCore                       --
--                                                                          --
-- Couverture is free software; you can redistribute it  and/or modify it   --
-- under terms of the GNU General Public License as published by the Free   --
-- Software Foundation; either version 2, or (at your option) any later     --
-- version.  Couverture is distributed in the hope that it will be useful,  --
-- but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHAN-  --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License  for more details. You  should  have  received a copy of the GNU --
-- General Public License  distributed with GNAT; see file COPYING. If not, --
-- write  to  the Free  Software  Foundation,  59 Temple Place - Suite 330, --
-- Boston, MA 02111-1307, USA.                                              --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Unchecked_Conversion;
with Ada.Text_IO;      use Ada.Text_IO;
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Directories;  use Ada.Directories;

with Interfaces;

with GNAT.OS_Lib;

with Coverage;     use Coverage;
with Qemu_Traces;
with Qemudrv_Base; use Qemudrv_Base;
with Switches;     use Switches;
with Traces_Files; use Traces_Files;

package body Qemudrv is

   Target_Default : constant String_Access := new String'("powerpc-elf");

   --  Variables set by the command line.

   Trace_Output : String_Access;
   --  Trace output filename

   Histmap_Filename : String_Access;
   --  File name of history map or null if none.

   Executable : String_Access;
   --  Executable to run

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
      Eargs    : String_List_Access)
   is
      Driver_Index : Integer;
      Real_Target  : not null String_Access := Target_Default;
      Nbr_Eargs    : Natural := 0;
   begin
      if Target /= null then
         Real_Target := Target;
      end if;

      if Output /= null then
         Trace_Output := Output;
      else
         Trace_Output := new String'(Simple_Name (Exe_File & ".trace"));
      end if;

      Histmap_Filename := Histmap;

      Executable := new String'(Exe_File);

      if Eargs /= null then
         Nbr_Eargs := Eargs'Length;
      end if;

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

         Write_Trace_File (Trace_Output.all, Trace_File);
         Free (Trace_File);
      end;

      --  Search for the driver

      Driver_Index := -1;
      for I in Drivers'Range loop
         if Drivers (I).Target.all = Real_Target.all then
            Driver_Index := I;
            exit;
         end if;
      end loop;

      if Driver_Index < 0 then
         Error ("unknown target " & Real_Target.all);
         Error (" (use --help to get target list)");
         --  ??? xcov run --help should give the target list
         return;
      end if;

      --  Build the executable (if necessary)

      if Drivers (Driver_Index).Build_Command /= null then
         Run_Command (Drivers (Driver_Index).Build_Command,
                      Drivers (Driver_Index).Build_Options.all);
      end if;

      --  The 'prepare' target do not launch qemu

      if Drivers (Driver_Index).Run_Command = null then
         return;
      end if;

      --  Run qemu

      declare
         Driver : Driver_Target renames Drivers (Driver_Index);
         L : constant Natural := Driver.Run_Options'Length;
         Opts : String_List (1 .. L + Nbr_Eargs);
      begin
         Opts (1 .. L) := Driver.Run_Options.all;

         if Eargs /= null then
            Opts (L + 1 .. L + Nbr_Eargs) := Eargs (1 .. Nbr_Eargs);
         end if;

         Run_Command (Driver.Run_Command, Opts);

         if Verbose then
            Put (Driver.Run_Command.all & " finished");
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

   procedure Help (Indent : String := "")
   is
      procedure P (Str : String);
      --  Put_Line with proper indentation

      -------
      -- P --
      -------

      procedure P (Str : String) is
      begin
         Put_Line (Indent & Str);
      end P;

   begin
      Put ("Usage: xcov run");
      Put (" [OPTIONS] FILE [-eargs EARGS...]");
      New_Line;
      P ("  Options are:");
      P ("  -t TARGET  --target=TARGET   Set the target");
      Put (Indent & "    targets:");
      for I in Drivers'Range loop
         Put (' ');
         Put (Drivers (I).Target.all);
      end loop;
      New_Line;
      P ("  -v --verbose                 Be verbose");
      P ("  -T TAG  --tag=TAG            Put TAG in tracefile");
      P ("  -o FILE  --output=FILE       Write traces to FILE");
      P ("  -eargs EARGS                 Pass EARGS to the simulator");
   end Help;

   -----------------
   -- Run_Command --
   -----------------

   procedure Run_Command (Command : String_Access; Options : String_List) is
      Args    : String_List (1 .. Options'Length) := Options;
      Success : Boolean;
      Prg     : String_Access;
   begin
      --  Find executable

      Prg := GNAT.OS_Lib.Locate_Exec_On_Path (Command.all);
      if Prg = null then
         Error ("xcov run: cannot find "
                  & Command.all
                  & " on your path");
         raise Exec_Error;
      end if;

      --  Copy arguments and expand argument macros

      for J in Args'Range loop
         if Args (J).all = "$exe" then
            Args (J) := Executable;

         elsif Args (J).all = "$bin" then
            Args (J) := new String'(Executable.all & ".bin");

         elsif Args (J).all = "$dir_exe" then
            Args (J) := new String'(Containing_Directory (Executable.all));

         elsif Args (J).all = "$base_bin" then
            Args (J) := new String'(Simple_Name (Executable.all) & ".bin");

         elsif Args (J).all = "$trace" then
            if Histmap_Filename /= null then
               Args (J) := new String'("histmap=" & Histmap_Filename.all & ','
                                         & Trace_Output.all);
            elsif Enabled (MCDC) then
               Args (J) := new String'("history," & Trace_Output.all);
            else
               Args (J) := Trace_Output;
            end if;
         end if;
      end loop;

      if Verbose then
         Put ("exec: ");
         Put (Prg.all);
         for I in Args'Range loop
            Put (' ');
            Put (Args (I).all);
         end loop;
         New_Line;
      end if;

      --  Run

      GNAT.OS_Lib.Spawn (Prg.all, Args, Success);
      if not Success then
         if Verbose then
            Error ("xcov run failed");
         end if;
         raise Exec_Error;
      end if;
   end Run_Command;

end Qemudrv;
