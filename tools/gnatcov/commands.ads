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

package Commands is
   --  This package provide basic support to manipulate xcov's commands

   type Command_Type is
     (No_Command,
      Cmd_Help,
      Cmd_Help_Dump,
      Cmd_Version,
      Cmd_Coverage,
      Cmd_Disp_Routines,
      Cmd_Map_Routines,
      Cmd_Check_SCOs,
      Cmd_Dump_Trace,
      Cmd_Dump_Trace_Raw,
      Cmd_Dump_Trace_Base,
      Cmd_Dump_Trace_Asm,
      Cmd_Dump_Sections,
      Cmd_Dump_Symbols,
      Cmd_Dump_Compile_Units,
      Cmd_Dump_Subprograms,
      Cmd_Dump_Lines,
      Cmd_Disassemble_Raw,
      Cmd_Disassemble,
      Cmd_Run,
      Cmd_Scan_Objects
     );

   function To_Command (Opt_String : String) return Command_Type;
   --  Convert a string of the form "com-mand" to the corresponding
   --  Command literal Com_Mand. No_Command is returned when no matching
   --  literal exists.

   function To_Switch (Command : Command_Type) return String;
   --  Return the command-line switch form of Command

   function For_Command_Switch (Command : Command_Type) return String;
   --  Generate command indication if Command is not No_Command

   type Command_Array is array (Positive range <>) of Command_Type;

   procedure Check_Option
     (Option            : String;
      Command           : Command_Type;
      Accepted_Commands : Command_Array);
   --  For a given option (labeled Option), check if it makes sense in the
   --  context of Command; fatal error otherwise.
   --  Accepted_Commands lists the commands that supports this option.

end Commands;
