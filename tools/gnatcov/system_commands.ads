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

with Ada.Strings.Unbounded;

with Strings; use Strings;

package System_Commands is

   type Command_Type is record
      Command : Ada.Strings.Unbounded.Unbounded_String;
      --  Command to run or empty string if there is no command to run

      Arguments : String_Vectors.Vector;
      --  Arguments to pass to this command

      Environment : String_Maps.Map;
      --  Environment variables to set for this command
   end record;
   --  Simple holder for a command to run

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
      In_To_Null          : Boolean := False) return Boolean;
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
   --  output stream iff Err_To_Out is True.
   --
   --  The subprocess standard input stream is redirected to the "null" stream
   --  (stream where there is nothing to read, equivalent to /dev/null on Unix
   --  systems) iff In_To_Null is True.

   function Run_Command
     (Command             : String;
      Arguments           : String_Vectors.Vector;
      Environment         : String_Maps.Map := Empty_Environment;
      Origin_Command_Name : String;
      Output_File         : String := "";
      Err_To_Out          : Boolean := True;
      In_To_Null          : Boolean := False) return Boolean;
   --  Overload to avoid the Command_Type layer

end System_Commands;
