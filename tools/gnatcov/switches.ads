------------------------------------------------------------------------------
--                                                                          --
--                               GNATcoverage                               --
--                                                                          --
--                     Copyright (C) 2009-2015, AdaCore                     --
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

with Ada.Containers.Indefinite_Ordered_Maps;

with GNAT.Strings; use GNAT.Strings;

package Switches is

   Verbose : Boolean := False;
   --  Verbose informational output

   All_Decisions : Boolean := False;
   --  If True, perform decision coverage in stmt+decision mode even for
   --  decisions outside of control structures.

   All_Messages : Boolean := False;
   --  If True, then when performing source coverage analysis, also include in
   --  the report messages other than violations of a source coverage
   --  obligation.

   Recursive_Projects : Boolean := False;
   --  When a project file is specified using -P, also consider all imported
   --  projects for coverage.

   Branch_Stats : Boolean := False;
   --  If True, dump statistics about branch instructions after the static
   --  analysis pass.

   Excluded_SCOs : Boolean := False;
   --  If True, report SCOs whose coverage cannot be established due to
   --  absence of executable code.

   type Separated_Source_Coverage_Type is (None, Routines, Instances);
   Separated_Source_Coverage : Separated_Source_Coverage_Type := None;

   -------------------------------------------------------------
   -- Project related switches that may need to be propagated --
   -------------------------------------------------------------

   Root_Project : String_Access := null;
   --  Project name as specified to the -P option of the command line.

   package Key_Element_Maps is
     new Ada.Containers.Indefinite_Ordered_Maps
       (Key_Type     => String,
        Element_Type => String);

   S_Variables : Key_Element_Maps.Map;
   --  All defined scenario variables, as provided through -X options on
   --  the command line.

   ------------------------------
   -- Debugging switches (-d?) --
   ------------------------------

   type Debug_Type is
     (None,

      --  Break long instructions in disassemblies, a la objdump.
      Break_Long_Instructions,

      --  Keep full historical traces for MC/DC even for decisions that do not
      --  require it (decisions without diamond paths).
      Full_History,

      --  Exemption pragmas have no effect.
      Ignore_Exemptions,

      --  Show debugging output for files table management
      File_Table);
   --  Set of debug switches to expose on command-line

   subtype Valid_Debug_Type is Debug_Type range
      Break_Long_Instructions .. File_Table;

   Debug_Switches : array (Valid_Debug_Type) of Boolean := (others => False);
   --  For each debug switches, tell whether it's enabled

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

   --  Convenience shortcuts:

   Debug_Break_Long_Instructions : Boolean renames
      Debug_Switches (Break_Long_Instructions);

   Debug_Full_History : Boolean renames
      Debug_Switches (Full_History);

   Debug_Ignore_Exemptions : Boolean renames
      Debug_Switches (Ignore_Exemptions);

   Debug_File_Table : Boolean renames
      Debug_Switches (File_Table);

end Switches;
