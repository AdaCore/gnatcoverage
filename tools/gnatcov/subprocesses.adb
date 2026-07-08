------------------------------------------------------------------------------
--                                                                          --
--                               GNATcoverage                               --
--                                                                          --
--                     Copyright (C) 2008-2024, AdaCore                     --
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

with Outputs; use Outputs;

package body Subprocesses is

   function Run_Command
     (Command             : String;
      Arguments           : String_Vectors.Vector;
      Origin_Command_Name : String;
      Environment         : String_Maps.Map := Empty_Environment;
      Output_File         : String := "";
      Err_To_Out          : Boolean := True;
      Out_To_Null         : Boolean := False;
      In_To_Null          : Boolean := False) return Process_Handle;
   --  Overload to run asynchronously a command

   procedure Print_On_Stderr (Filename : String);
   --  If Filename is not empty, read the content of the given file and forward
   --  it to the standard error stream.

   procedure Check_Status
     (Success             : Boolean;
      Output_File         : String;
      Ignore_Error        : Boolean;
      Command             : String;
      Origin_Command_Name : String);
   --  If Ignore_Error is False and Success is False, raise a Fatal_Error.
   --
   --  In addition, in case of failure and Output_File is not empty, forward
   --  its content to the standard error strem.

   ---------
   -- "=" --
   ---------

   function "=" (L, R : Command_Type) return Boolean is
      use String_Vectors;
   begin
      return L.Command = R.Command and then L.Arguments = R.Arguments;
   end "=";

   ----------
   -- Read --
   ----------

   procedure Read
     (CLS : in out Checkpoints.Checkpoint_Load_State; Value : out Command_Type)
   is
   begin
      Value.Command := CLS.Read_Unbounded_String;
      Read (CLS, Value.Arguments);
      Read (CLS, Value.Environment);
   end Read;

   -----------
   -- Write --
   -----------

   procedure Write
     (CSS : in out Checkpoints.Checkpoint_Save_State; Value : Command_Type) is
   begin
      CSS.Write (Value.Command);
      Write (CSS, Value.Arguments);
      Write (CSS, Value.Environment);
   end Write;

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

   ---------------------
   -- Print_On_Stderr --
   ---------------------

   procedure Print_On_Stderr (Filename : String) is
      F : File_Type;
   begin
      if Filename /= "" then
         Open (F, In_File, Filename);
         while not End_Of_File (F) loop
            Warning_Or_Error (Get_Line (F));
         end loop;
         Close (F);
      end if;
   end Print_On_Stderr;

   ------------------
   -- Check_Status --
   ------------------

   procedure Check_Status
     (Success             : Boolean;
      Output_File         : String;
      Ignore_Error        : Boolean;
      Command             : String;
      Origin_Command_Name : String) is
   begin
      if not Ignore_Error and then not Success then
         Print_On_Stderr (Output_File);
         Fatal_Error (Origin_Command_Name & " failed: aborting");

      elsif Subprocesses_Trace.Is_Active then
         if Success then
            Subprocesses_Trace.Trace (Command & " finished");
         else
            --  Do not use Error as this sets the exit status to Failure, but
            --  here we are precisely ignoring the fact that the subprocess
            --  failed.

            Print_On_Stderr (Output_File);
            Warning_Or_Error (Origin_Command_Name & " failed");
         end if;
      end if;
   end Check_Status;

   -----------------
   -- Run_Command --
   -----------------

   function Run_Command
     (Command             : Command_Type;
      Origin_Command_Name : String;
      Output_File         : String := "";
      Err_To_Out          : Boolean := True;
      Out_To_Null         : Boolean := False;
      In_To_Null          : Boolean := False;
      Ignore_Error        : Boolean := False) return Boolean is
   begin
      return
        Run_Command
          (+Command.Command,
           Command.Arguments,
           Origin_Command_Name,
           Command.Environment,
           Output_File,
           Err_To_Out,
           Out_To_Null,
           In_To_Null,
           Ignore_Error);
   end Run_Command;

   procedure Run_Command
     (Command             : Command_Type;
      Origin_Command_Name : String;
      Output_File         : String := "";
      Err_To_Out          : Boolean := True;
      Out_To_Null         : Boolean := False;
      In_To_Null          : Boolean := False)
   is
      Dummy : constant Boolean :=
        Run_Command
          (Command,
           Origin_Command_Name,
           Output_File,
           Err_To_Out,
           Out_To_Null,
           In_To_Null,
           Ignore_Error => False);
   begin
      null;
   end Run_Command;

   procedure Run_Command
     (Command             : String;
      Arguments           : String_Vectors.Vector;
      Origin_Command_Name : String;
      Environment         : String_Maps.Map := Empty_Environment;
      Output_File         : String := "";
      Err_To_Out          : Boolean := True;
      Out_To_Null         : Boolean := False;
      In_To_Null          : Boolean := False)
   is
      Dummy : constant Boolean :=
        Run_Command
          (Command,
           Arguments,
           Origin_Command_Name,
           Environment,
           Output_File,
           Err_To_Out,
           Out_To_Null,
           In_To_Null,
           Ignore_Error => False);
   begin
      null;
   end Run_Command;

   function Run_Command
     (Command             : String;
      Arguments           : String_Vectors.Vector;
      Origin_Command_Name : String;
      Environment         : String_Maps.Map := Empty_Environment;
      Output_File         : String := "";
      Err_To_Out          : Boolean := True;
      Out_To_Null         : Boolean := False;
      In_To_Null          : Boolean := False;
      Ignore_Error        : Boolean := False) return Boolean
   is
      Handle  : constant Process_Handle :=
        Run_Command
          (Command,
           Arguments,
           Origin_Command_Name,
           Environment,
           Output_File,
           Err_To_Out,
           Out_To_Null,
           In_To_Null);
      Success : constant Boolean :=
        Wait (Handle) = 0 and then Handle /= Invalid_Handle;
   begin
      Check_Status
        (Success, Output_File, Ignore_Error, Command, Origin_Command_Name);
      return Success;
   end Run_Command;

   function Run_Command
     (Command             : String;
      Arguments           : String_Vectors.Vector;
      Origin_Command_Name : String;
      Environment         : String_Maps.Map := Empty_Environment;
      Output_File         : String := "";
      Err_To_Out          : Boolean := True;
      Out_To_Null         : Boolean := False;
      In_To_Null          : Boolean := False) return Process_Handle
   is
      package Process_Types renames GNATCOLL.OS.Process_Types;

      Program : String_Access;
      Env     : Process_Types.Environ;
      Args    : Process_Types.Arguments;
   begin
      if Out_To_Null and then Output_File /= "" then
         raise Program_Error;
      end if;

      --  Find the actual executable to execute

      Program := GNAT.OS_Lib.Locate_Exec_On_Path (Command);
      if Program = null then
         Error
           (Origin_Command_Name
            & ": cannot find "
            & Command
            & " on your path");
         return Invalid_Handle;
      end if;

      --  Instantiate environment variables

      Process_Types.Import (Env);
      if not Environment.Is_Empty then
         Subprocesses_Trace.Trace ("env:");
      end if;
      for Env_Var in Environment.Iterate loop
         declare
            Name  : constant String := +String_Maps.Key (Env_Var);
            Value : constant String := +String_Maps.Element (Env_Var);
         begin
            Subprocesses_Trace.Trace ("  " & Name & "='" & Value & "'");
            Process_Types.Set_Variable (Env, Name, Value);
         end;
      end loop;

      --  Instantiate the argument list

      declare
         Log : Unbounded_String;
      begin
         Process_Types.Add_Argument (Args, Program.all);
         Append (Log, "exec:");
         Append (Log, " '" & Program.all & "'");
         Free (Program);

         for A of Arguments loop
            Process_Types.Add_Argument (Args, +A);

            --  Quote the arguments to print empty strings and correctly
            --  escape quoted strings.

            Append (Log, " '");
            Append (Log, A);
            Append (Log, "'");
         end loop;
         Subprocesses_Trace.Trace (+Log);
      end;

      --  Actually run the subprocess

      declare
         Stdin          : constant File_Descriptor :=
           (if In_To_Null then Null_FD else Standin);
         Stdout, Stderr : File_Descriptor;
         Handle         : Process_Handle;

         Redirect_Stdout : constant Boolean := Output_File /= "";
      begin
         --  If requested, redirect the subprocess' standard output to a file

         if Out_To_Null then
            Stdout := Null_FD;
         elsif Redirect_Stdout then
            Stdout := Open (Output_File, Write_Mode);
            if Stdout = Invalid_FD then
               Fatal_Error ("cannot write to " & Output_File);
            end if;
         else
            Stdout := Standout;
         end if;
         Stderr := (if Err_To_Out then Stdout else Standerr);

         Handle :=
           Start
             (Args   => Args,
              Env    => Env,
              Stdin  => Stdin,
              Stdout => Stdout,
              Stderr => Stderr);

         Process_Types.Deallocate (Args);
         Process_Types.Deallocate (Env);
         if Redirect_Stdout then
            Close (Stdout);
         end if;
         return Handle;
      end;
   end Run_Command;

end Subprocesses;
