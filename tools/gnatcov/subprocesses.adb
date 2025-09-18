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

with GNATCOLL.Mmap;  use GNATCOLL.Mmap;
with GNATCOLL.OS.FS; use GNATCOLL.OS.FS;
with GNATCOLL.OS.Process_Types;

with Outputs; use Outputs;
with Paths;   use Paths;

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

   function Wait_And_Finalize (Self : in out Process_Pool) return Positive;
   --  Wait for a process to terminate and handle its output. Return the id in
   --  Self for the process that terminated.

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

   -----------------------
   -- Wait_And_Finalize --
   -----------------------

   function Wait_And_Finalize (Self : in out Process_Pool) return Positive is
      function Find_Waitable return Positive;
      --  Wait until one process terminates, then return its index in
      --  Self.Handle.

      -------------------
      -- Find_Waitable --
      -------------------

      function Find_Waitable return Positive is
         Result : Integer := WAIT_TIMEOUT;
      begin
         --  Use a long timeout (one hour) to avoid busy polling instead of
         --  using INFINITE_TIMEOUT to workaround a GNATCOLL.OS.Process bug.

         while Result = WAIT_TIMEOUT loop
            Result := Wait_For_Processes (Self.Handles, 3600.0);
         end loop;
         pragma Assert (Result in Self.Handles'Range);
         return Result;
      end Find_Waitable;

      Id      : constant Positive := Find_Waitable;
      Success : constant Boolean := Wait (Self.Handles (Id)) = 0;
      Info    : Process_Info renames Self.Process_Infos (Id);

      --  Start of processing for Wait_And_Finalize

   begin
      --  Free the pool slot for this terminated process

      Self.Handles (Id) := Invalid_Handle;
      Self.Nb_Running_Processes := Self.Nb_Running_Processes - 1;

      --  Dump the output of the process that terminated to stdout if it was
      --  not redirected to a file.

      if Info.Output_To_Stdout then
         declare
            Output_File : Mapped_File;
            Region      : Mapped_Region;
            Str         : Str_Access;
            Str_Last    : Natural;
         begin
            Output_File := Open_Read (+Info.Output_File);
            Region := Read (Output_File);
            Str := Data (Region);
            Str_Last := Last (Region);

            --  The finalization of Ada.Text_IO always emits a newline after a
            --  call to Put. To avoid redundant line breaks in this situation,
            --  do not pass that newline to Put and call New_Line instead.

            if Str_Last > 0 then
               if Str (Str_Last) = ASCII.LF then
                  Str_Last := Str_Last - 1;
               end if;
               Put (Str (1 .. Str_Last));
               New_Line;
            end if;

            Free (Region);
            Close (Output_File);
         end;
      end if;

      --  If the subprocess terminated with an error, deal with it here

      declare
         Output_File : constant String :=
           (if Info.Output_To_Stdout then "" else +Info.Output_File);
      begin
         Check_Status
           (Success,
            Output_File,
            Info.Ignore_Error,
            +Info.Command,
            +Info.Origin_Command_Name);
      end;
      return Id;
   end Wait_And_Finalize;

   -----------------
   -- Run_Command --
   -----------------

   procedure Run_Command
     (Pool                : in out Process_Pool;
      Command             : String;
      Arguments           : String_Vectors.Vector;
      Origin_Command_Name : String;
      Environment         : String_Maps.Map := Empty_Environment;
      Output_File         : String := "";
      Err_To_Out          : Boolean := True;
      Out_To_Null         : Boolean := False;
      In_To_Null          : Boolean := False;
      Ignore_Error        : Boolean := False)
   is
      Id : Positive;
      --  Identifier of the process in the process pool (i.e. its index in
      --  the Pool.Handles process array).

   begin
      --  If the process pool is full, wait for the first completion to occur
      --  and create the new process to replace it. Otherwise we just started
      --  to fill it: use the next available slot.

      if Pool.Nb_Running_Processes = Pool.Parallelism_Level then
         Id := Pool.Wait_And_Finalize;
      else
         Id := Pool.Nb_Running_Processes + 1;
      end if;

      --  Fill the information relative to this command that we will need
      --  when it terminates.

      Pool.Process_Infos (Id).Ignore_Error := Ignore_Error;
      Pool.Process_Infos (Id).Output_File := +Output_File;
      Pool.Process_Infos (Id).Origin_Command_Name := +Origin_Command_Name;
      Pool.Process_Infos (Id).Command := +Command;
      if Output_File = "" then

         --  Redirect the output to a temporary file to avoid mangling
         --  on the standard output. The dump of the output is done in
         --  Wait_And_Finalize, through a later call to Run_Command, or
         --  through the finalization of the process pool.

         Pool.Process_Infos (Id).Output_File :=
           +(Pool.Output_Dir.Directory_Name / "job-"
             & Strings.Img (Id)
             & ".txt");
         Pool.Process_Infos (Id).Output_To_Stdout := True;
      else
         Pool.Process_Infos (Id).Output_To_Stdout := False;
      end if;

      Pool.Handles (Id) :=
        Run_Command
          (Command,
           Arguments,
           Origin_Command_Name,
           Environment,
           +Pool.Process_Infos (Id).Output_File,

           --  TODO??? There will be mangling on the stderr if stdout was
           --  redirected to an output file.

           Err_To_Out  => Output_File = "",
           Out_To_Null => Out_To_Null,
           In_To_Null  => In_To_Null);
      Pool.Nb_Running_Processes := Pool.Nb_Running_Processes + 1;
   end Run_Command;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Pool : in out Process_Pool) is
   begin
      Create_Temporary_Directory (Pool.Output_Dir, "gnatcov_outputs");
      Pool.Handles := (others => Invalid_Handle);
      Pool.Nb_Running_Processes := 0;
   end Initialize;

   --------------
   -- Finalize --
   --------------

   overriding
   procedure Finalize (Self : in out Process_Pool) is
      Dummy : Positive;
   begin
      while Self.Nb_Running_Processes /= 0 loop
         Dummy := Wait_And_Finalize (Self);
      end loop;
   end Finalize;

end Subprocesses;
