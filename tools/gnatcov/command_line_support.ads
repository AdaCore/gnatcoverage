------------------------------------------------------------------------------
--                                                                          --
--                               GNATcoverage                               --
--                                                                          --
--                     Copyright (C) 2009-2022, AdaCore                     --
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

package Command_Line_Support is

   ------------------------------
   -- Debugging switches (-d?) --
   ------------------------------

   --  Set of debug switches to expose on command-line

   type Debug_Type is
     (None,

      Break_Long_Instructions,
      --  Break long instructions in disassemblies, a la objdump

      Full_History,
      --  Keep full historical traces for MC/DC even for decisions that do not
      --  require it (decisions without multi-path conditions).

      Ignore_Exemptions,
      --  Exemption pragmas have no effect.

      File_Table);
      --  Show debugging output for files table management

   subtype Valid_Debug_Type is Debug_Type range
      Break_Long_Instructions .. File_Table;
   --  Set of debug switches to expose on command-line

   Debug_Switches_Map : constant array (Character) of Debug_Type :=
     ('b'    => Break_Long_Instructions,
      'h'    => Full_History,
      'i'    => Ignore_Exemptions,
      'f'    => File_Table,
      others => None);
   --  Map characters to switches. Update this map to make a debug flags
   --  available through the -d command-line argument (see command_line.ads).

   function Debug_Command_Line_Pattern return String;
   --  Return a pattern string for the usage of the command-line -d option

   function Debug_Command_Line_Help return String;
   --  Return a help string for the usage of the command-line -d option

end Command_Line_Support;
