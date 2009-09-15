------------------------------------------------------------------------------
--                                                                          --
--                              Couverture                                  --
--                                                                          --
--                       Copyright (C) 2009, AdaCore                        --
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

package Commands is
   --  This package provide basic support to manipulate xcov's commands

   type Command_Type is
     (No_Command,
      Coverage,
      Disp_Routines,
      Map_Routines,
      Dump_Trace,
      Dump_Trace_Base,
      Dump_Trace_Asm,
      Dump_Sections,
      Dump_Symbols,
      Dump_Compile_Units,
      Dump_Subprograms,
      Dump_Lines,
      Disassemble_Raw,
      Disassemble,
      Show_Graph,
      Run);

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
