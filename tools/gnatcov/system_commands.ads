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
with Ada.Unchecked_Deallocation;

with Strings; use Strings;

package System_Commands is

   type Command_Type is record
      Command : Ada.Strings.Unbounded.Unbounded_String;
      --  Command to run or empty string if there is no command to run

      Arguments : String_Vectors.Vector;
      --  Arguments to pass to this command

      Input : Ada.Strings.Unbounded.Unbounded_String;
      --  Input to pass to this command if it is needed

      Environment : String_Maps.Map;
      --  Environment variables to set for this command

      Native : Boolean;
      --  Whether this command will run a native program
   end record;
   --  Simple holder for a command to run

   type Command_Access is access all Command_Type;

   procedure Append_Arg (Cmd : Command_Access; Arg : String);

   procedure Append_Arg (Cmd : Command_Access; Opt, Arg : String);

   procedure Append_Args (Cmd : Command_Access; Args : String_Vectors.Vector);

   procedure Free is new Ada.Unchecked_Deallocation
     (Command_Type, Command_Access);

   procedure Error (Msg : String);

   Exec_Error : exception;
   --  Raised when subprogram execution failed. The error message shall be
   --  generated before raising the exception.

   procedure Run_Command
     (Command             : Command_Type;
      Origin_Command_Name : String;
      Output_File         : String := "";
      Err_To_Out          : Boolean := True);
end System_Commands;
