------------------------------------------------------------------------------
--                                                                          --
--                               GNATcoverage                               --
--                                                                          --
--                     Copyright (C) 2008-2022, AdaCore                     --
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

with Ada.Text_IO; use Ada.Text_IO;

with GNAT.OS_Lib;
with GNAT.Strings; use GNAT.Strings;

with GNATCOLL.OS.FS;      use GNATCOLL.OS.FS;
with GNATCOLL.OS.Process; use GNATCOLL.OS.Process;
with GNATCOLL.OS.Process_Types;

with Outputs;  use Outputs;
with Switches; use Switches;

package body Subprocesses is

   ----------------
   -- Append_Arg --
   ----------------

   procedure Append_Arg (Cmd : in out Command_Type; Arg : String) is
   begin
      Cmd.Arguments.Append (+Arg);
   end Append_Arg;

   procedure Append_Arg (Cmd : in out Command_Type; Opt, Arg : String) is
   begin
      Append_Arg (Cmd, Opt);
      Append_Arg (Cmd, Arg);
   end Append_Arg;

   procedure Append_Args
     (Cmd : in out Command_Type; Args : String_Vectors.Vector) is
   begin
      Cmd.Arguments.Append_Vector (Args);
   end Append_Args;

   -----------------
   -- Run_Command --
   -----------------

   function Run_Command
     (Command             : Command_Type;
      Origin_Command_Name : String;
      Output_File         : String := "";
      Err_To_Out          : Boolean := True;
      In_To_Null          : Boolean := False;
      Ignore_Error        : Boolean := False) return Boolean
   is
   begin
      return Run_Command
        (+Command.Command,
         Command.Arguments,
         Command.Environment,
         Origin_Command_Name,
         Output_File,
         Err_To_Out,
         In_To_Null,
         Ignore_Error);
   end Run_Command;

   procedure Run_Command
     (Command             : Command_Type;
      Origin_Command_Name : String;
      Output_File         : String := "";
      Err_To_Out          : Boolean := True;
      In_To_Null          : Boolean := False)
   is
      Dummy : constant Boolean := Run_Command
        (Command,
         Origin_Command_Name,
         Output_File,
         Err_To_Out,
         In_To_Null,
         Ignore_Error => False);
   begin
      null;
   end Run_Command;

   procedure Run_Command
     (Command             : String;
      Arguments           : String_Vectors.Vector;
      Environment         : String_Maps.Map := Empty_Environment;
      Origin_Command_Name : String;
      Output_File         : String := "";
      Err_To_Out          : Boolean := True;
      In_To_Null          : Boolean := False)
   is
      Dummy : constant Boolean := Run_Command
        (Command,
         Arguments,
         Environment,
         Origin_Command_Name,
         Output_File,
         Err_To_Out,
         In_To_Null,
         Ignore_Error => False);
   begin
      null;
   end Run_Command;

   function Run_Command
     (Command             : String;
      Arguments           : String_Vectors.Vector;
      Environment         : String_Maps.Map := Empty_Environment;
      Origin_Command_Name : String;
      Output_File         : String := "";
      Err_To_Out          : Boolean := True;
      In_To_Null          : Boolean := False;
      Ignore_Error        : Boolean := False) return Boolean
   is
      package Process_Types renames GNATCOLL.OS.Process_Types;

      Program : String_Access;
      Env     : Process_Types.Environ;
      Args    : Process_Types.Arguments;
      Success : Boolean;
   begin

      --  Honor a possible empty command text, meaning no actual
      --  command to run.

      if Command'Length = 0 then
         return True;
      end if;

      --  Find the actual executable to execute

      Program := GNAT.OS_Lib.Locate_Exec_On_Path (Command);
      if Program = null then
         Error (Origin_Command_Name & ": cannot find "
                & Command & " on your path");
         return False;
      end if;

      --  Instantiate environment variables

      Process_Types.Import (Env);
      if Verbose and then not Environment.Is_Empty then
         Put_Line ("env:");
      end if;
      for Env_Var in Environment.Iterate loop
         declare
            Name  : constant String := +String_Maps.Key (Env_Var);
            Value : constant String := +String_Maps.Element (Env_Var);
         begin
            if Verbose then
               Put_Line ("  " & Name & "='" & Value & "'");
            end if;
            Process_Types.Set_Variable (Env, Name, Value);
         end;
      end loop;

      --  Instantiate the argument list

      Process_Types.Add_Argument (Args, Program.all);
      if Verbose then
         Put_Line ("exec:");
         Put ("  " & Program.all);
      end if;
      Free (Program);

      for A of Arguments loop
         Process_Types.Add_Argument (Args, +A);
         if Verbose then
            Put (' ');
            Put (+A);
         end if;
      end loop;
      if Verbose then
         New_Line;
      end if;

      --  Actually run the subprocess

      declare
         Stdin  : constant File_Descriptor :=
           (if In_To_Null then Null_FD else Standin);
         Stdout : File_Descriptor := Standout;
         Stderr : constant File_Descriptor :=
           (if Err_To_Out then To_Stdout else Standerr);
         Status : Integer;

         Redirect_Stdout : constant Boolean := Output_File /= "";
      begin
         --  If requested, redirect the subprocess' standard output to a file

         if Redirect_Stdout then
            Stdout := Open (Output_File, Write_Mode);
            if Stdout = Invalid_FD then
               Fatal_Error ("cannot write to " & Output_File);
            end if;
         end if;

         Status := Run
           (Args   => Args,
            Env    => Env,
            Stdin  => Stdin,
            Stdout => Stdout,
            Stderr => Stderr);
         Success := Status = 0;

         Process_Types.Deallocate (Args);
         Process_Types.Deallocate (Env);
         if Redirect_Stdout then
            Close (Stdout);
         end if;
      end;

      if not Ignore_Error and then not Success then
         Fatal_Error (Origin_Command_Name & " failed: aborting");

      elsif Verbose then
         if Success then
            Put_Line (Command & " finished");
         else
            Error (Origin_Command_Name & " failed");
         end if;
      end if;
      return Success;
   end Run_Command;

end Subprocesses;
