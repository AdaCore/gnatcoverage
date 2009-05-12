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
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Directories; use Ada.Directories;
with Interfaces;
with GNAT.Command_Line; use GNAT.Command_Line;
--  with GNAT.Strings; use GNAT.Strings;
with GNAT.OS_Lib; use GNAT.OS_Lib;
with Traces_Files; use Traces_Files;
with Qemu_Traces;
with Qemudrv_Base; use Qemudrv_Base;

package body Qemudrv is

   Progname : String_Access;
   --  Name of the program.  Used in error messages.

   --  Variables set by the command line.

   Target : String_Access := new String'("powerpc-elf");
   --  Target to use (AdaCore name).

   Output : String_Access;
   --  Trace output filename.

   Verbose : Boolean := False;
   --  Verbose (display more messages).

   Exe_File : String_Access;
   --  Executable to run.

   Getopt_Switches : constant String :=
     "v -verbose t: -target= o: -output= h -help -tag= -T:";
   --  String for Getopt.

   Tag : String_Access;
   --  Tag to write in the trace file.

   Exec_Error : exception;
   --  Raised when subprogram execution failed.  The error message shall be
   --  generated before raising the exception.

   procedure Error (Msg : String);
   --  Display the message on the error output and set exit status.

   procedure Run_Command (Command : String_Access;
                          Options : String_List);
   --  Spawn command with options.

   procedure Error (Msg : String) is
   begin
      Put_Line (Standard_Error, Msg);
      Set_Exit_Status (Failure);
   end Error;

   procedure Help (Indent : String := "")
   is
      procedure P (Str : String);

      procedure P (Str : String) is
      begin
         Put_Line (Indent & Str);
      end P;
   begin
      if not Is_Xcov then
         Put ("Usage: " & Progname.all);
      else
         Put (Indent & "--run");
      end if;
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

   procedure Run_Command (Command : String_Access;
                          Options : String_List)
   is
      Args : String_List (1 .. Options'Length) := Options;
      Success : Boolean;
      Prg : String_Access;
   begin
      --  Find executable.
      Prg := Locate_Exec_On_Path (Command.all);
      if Prg = null then
         Error (Progname.all & ": cannot find "
                  & Command.all
                  & " on your path");
         raise Exec_Error;
      end if;

      --  Copy arguments and replace meta-one.
      for J in Args'Range loop
         if Args (J).all = "$exe" then
            Args (J) := Exe_File;
         elsif Args (J).all = "$bin" then
            Args (J) := new String'(Exe_File.all & ".bin");
         elsif Args (J).all = "$dir_exe" then
            Args (J) := new String'(Containing_Directory (Exe_File.all));
         elsif Args (J).all = "$base_bin" then
            Args (J) := new String'(Simple_Name (Exe_File.all) & ".bin");
         elsif Args (J).all = "$trace" then
            Args (J) := Output;
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

      --  Run.
      Spawn (Prg.all, Args, Success);
      if not Success then
         if Verbose then
            Error (Progname.all & " failed");
         end if;
         raise Exec_Error;
      end if;
   end Run_Command;

   procedure Driver (First_Option : Natural := 1)
   is
      Arg_Count : constant Natural := Argument_Count;

      subtype Opt_Index is Natural range 1 .. Arg_Count - First_Option + 1;
      Parser : Opt_Parser;
      Args : constant String_List_Access := new String_List (Opt_Index);
      Eargs : constant String_List_Access := new String_List (Opt_Index);
      Nbr_Eargs : Natural := 0;

      Driver_Index : Integer;
      S : Character;
   begin
      --  Set progname.
      if Is_Xcov then
         Progname := new String'(Command_Name & " --run");
      else
         Progname := new String'(Command_Name);
      end if;

      --  Build the command line.
      for I in First_Option .. Arg_Count loop
         Args (1 + I - First_Option) := new String'(Argument (I));
      end loop;

      --  And decode it.
      Initialize_Option_Scan (Parser, Args, Section_Delimiters => "eargs");
      loop
         S := Getopt (Getopt_Switches, False, Parser);
         exit when S = ASCII.NUL;

         if S = 'v'
           or else (S = '-' and then Full_Switch (Parser) = "-verbose")
         then
            Verbose := True;
         elsif S = 't'
           or else (S = '-' and then Full_Switch (Parser) = "-target")
         then
            Target := new String'(Parameter (Parser));
         elsif S = 'o'
           or else (S = '-' and then Full_Switch (Parser) = "-output")
         then
            Output := new String'(Parameter (Parser));
         elsif S = 'T'
           or else (S = '-' and then Full_Switch (Parser) = "-tag")
         then
            Tag := new String'(Parameter (Parser));
         elsif S = 'h'
           or else (S = '-' and then Full_Switch (Parser) = "-help")
         then
            Help;
            return;
         else
            raise Program_Error;
         end if;
      end loop;

      --  Exe file.
      declare
         S : constant String := Get_Argument (False, Parser);
      begin
         if S'Length = 0 then
            Error ("missing exe file to " & Progname.all);
            return;
         end if;
         Exe_File := new String'(S);
      end;

      --  Check for no extra arguments.
      declare
         S : constant String := Get_Argument (False, Parser);
      begin
         if S'Length /= 0 then
            Error ("too many arguments for " & Progname.all);
            return;
         end if;
      end;

      --  Simulator arguments.
      Goto_Section ("eargs", Parser);
      while Getopt ("*", False, Parser) /= ASCII.NUL loop
         Nbr_Eargs := Nbr_Eargs + 1;
         Eargs (Nbr_Eargs) := new String'(Full_Switch (Parser));
      end loop;

      Free (Parser);

      if Output = null then
         Output := new String'(Simple_Name (Exe_File.all & ".trace"));
      end if;

      --  Create the trace file.
      declare
         use Qemu_Traces;
         use Interfaces;
         Trace_File : Trace_File_Type;
         Date_Info  : Trace_Info_Date;
         Date       : constant OS_Time := Current_Time;
         subtype String_8 is String (1 .. 8);
         function Date_Info_To_Str is new Ada.Unchecked_Conversion
           (Trace_Info_Date, String_8);
      begin
         Create_Trace_File (Trace_File);
         Date_Info := Trace_Info_Date'(Year => Unsigned_16 (GM_Year (Date)),
                                       Month => Unsigned_8 (GM_Month (Date)),
                                       Day => Unsigned_8 (GM_Day (Date)),
                                       Hour => Unsigned_8 (GM_Hour (Date)),
                                       Min => Unsigned_8 (GM_Minute (Date)),
                                       Sec => Unsigned_8 (GM_Second (Date)),
                                       Pad => 0);
         Append_Info (Trace_File,
                      Info_Kind_Date, Date_Info_To_Str (Date_Info));

         Append_Info (Trace_File, Info_Kind_Exec_Filename, Exe_File.all);
         if Tag /= null then
            Append_Info (Trace_File, Info_Kind_User_Tag, Tag.all);
         end if;
         Write_Trace_File (Output.all, Trace_File);
         Free (Trace_File);
      end;

      --  Search for the driver.
      Driver_Index := -1;
      for I in Drivers'Range loop
         if Drivers (I).Target.all = Target.all then
            Driver_Index := I;
            exit;
         end if;
      end loop;

      if Driver_Index < 0 then
         Error (Progname.all & ": unknown target " & Target.all);
         Error (Progname.all & " (use --help to get target list)");
         return;
      end if;

      --  Build the executable (if necessary).
      if Drivers (Driver_Index).Build_Command /= null then
         Run_Command (Drivers (Driver_Index).Build_Command,
                      Drivers (Driver_Index).Build_Options.all);
      end if;

      --  The 'prepare' target do not launch qemu.
      if Drivers (Driver_Index).Run_Command = null then
         return;
      end if;

      --  Run qemu.
      declare
         Driver : Driver_Target renames Drivers (Driver_Index);
         L : constant Natural := Driver.Run_Options'Length;
         Opts : String_List (1 .. L + Nbr_Eargs);
      begin
         Opts (1 .. L) := Driver.Run_Options.all;
         Opts (L + 1 .. L + Nbr_Eargs) := Eargs (1 .. Nbr_Eargs);
         Run_Command (Driver.Run_Command, Opts);
         if Verbose then
            Put (Driver.Run_Command.all & " finished");
         end if;
      end;

   exception
      when Invalid_Switch =>
         Error (Progname.all
                  & ": invalid switch " & Full_Switch (Parser));
         return;
      when Invalid_Parameter =>
         Error (Progname.all
                  & ": missing parameter for " & Full_Switch (Parser));
         return;
      when Exec_Error =>
         return;
   end Driver;
end Qemudrv;
