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

--  This package provides a thin layer around the GNATCOLL.OS.Process API to
--  run subprocesses. This extra layer takes care of logging (in verbose mode,
--  subprocess arguments and environment variables), offers a simplified API to
--  handle gnatcov's small needs for I/O redirections and can automatically
--  abort gnatcov in case of subprocess failure.

with Ada.Strings.Unbounded;

with Strings; use Strings;

package Subprocesses is

   type Command_Type is record
      Command : Ada.Strings.Unbounded.Unbounded_String;
      --  Command to run or empty string if there is no command to run

      Arguments : String_Vectors.Vector;
      --  Arguments to pass to this command

      Environment : String_Maps.Map;
      --  Environment variables to set for this command. Note that the actual
      --  environment that is passed to the subprocess also inherits variables
      --  from gnatcov's own environment.
   end record;
   --  Simple holder for a command to run

   function "=" (L, R : Command_Type) return Boolean;
   --  Note: the equality comparison does not consider the commands'
   --  environment.

   Null_Command : constant Command_Type := (others => <>);

   procedure Append_Arg (Cmd : in out Command_Type; Arg : String);
   --  Append Arg to Cmd.Arguments

   procedure Append_Arg (Cmd : in out Command_Type; Opt, Arg : String);
   --  Append Opt and Arg to Cmd.Arguments

   procedure Append_Args
     (Cmd : in out Command_Type; Args : String_Vectors.Vector);
   --  Append all items in Args to Cmd.Arguments

   Empty_Environment : String_Maps.Map renames String_Maps.Empty_Map;

   function Run_Command
     (Command             : Command_Type;
      Origin_Command_Name : String;
      Output_File         : String := "";
      Err_To_Out          : Boolean := True;
      Out_To_Null         : Boolean := False;
      In_To_Null          : Boolean := False;
      Ignore_Error        : Boolean := False) return Boolean;
   --  Run the given command and return whether it exited with a zero status
   --  code (i.e. whether it was successful).
   --
   --  Origin_Command_Name is used as a short command name to include in error
   --  messages.
   --
   --  If Output_File is left to the empty string, the subprocess output is not
   --  redirected. Otherwise, it is redirected to create/overwrite the
   --  designated file.
   --
   --  The subprocess standard error stream is redirected to its standard
   --  output stream iff Err_To_Out is True. The standard output is itself
   --  redirected to the "null" stream iff Out_To_Null is True (stream where
   --  there is nothing to read, equivalent to /dev/null on Unix systems). In
   --  that case, it is an error for Output_File to be non-empty.
   --
   --  The subprocess standard input stream is redirected to the "null" stream
   --  iff In_To_Null is True.
   --
   --  If Ignore_Error is True and the subprocess exits with a non-zero status
   --  code, abort with Outputs.Fatal_Error.

   function Run_Command
     (Command             : String;
      Arguments           : String_Vectors.Vector;
      Environment         : String_Maps.Map := Empty_Environment;
      Origin_Command_Name : String;
      Output_File         : String := "";
      Err_To_Out          : Boolean := True;
      Out_To_Null         : Boolean := False;
      In_To_Null          : Boolean := False;
      Ignore_Error        : Boolean := False) return Boolean;
   --  Overload to avoid the Command_Type layer

   procedure Run_Command
     (Command             : Command_Type;
      Origin_Command_Name : String;
      Output_File         : String := "";
      Err_To_Out          : Boolean := True;
      Out_To_Null         : Boolean := False;
      In_To_Null          : Boolean := False);
   procedure Run_Command
     (Command             : String;
      Arguments           : String_Vectors.Vector;
      Environment         : String_Maps.Map := Empty_Environment;
      Origin_Command_Name : String;
      Output_File         : String := "";
      Err_To_Out          : Boolean := True;
      Out_To_Null         : Boolean := False;
      In_To_Null          : Boolean := False);
   --  Overloads to stop with a fatal error if the subprocess exits with a
   --  non-zero status code.

end Subprocesses;
