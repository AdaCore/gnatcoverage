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

   type Separated_Source_Coverage_Type is (None, Routines, Instances);
   Separated_Source_Coverage : Separated_Source_Coverage_Type := None;

   ------------------------------
   -- Debugging switches (-d?) --
   ------------------------------

   Debug_Full_History : Boolean := False;
   --  -dh
   --  Keep full historical traces for MC/DC even for decisions that do not
   --  require it (decisions without diamond paths).

   Debug_Ignore_Exemptions : Boolean := False;
   --  -di
   --  Exemption pragmas have no effect.

   Debug_Break_Long_Instructions : Boolean := False;
   --  --debug-break-long-instructions
   --  Break long instructions in disassemblies, a la objdump.

   -------------------------
   -- Sources of switches --
   -------------------------

   type Switches_Source is limited interface;
   function Argument_Count (S : Switches_Source) return Natural is abstract;
   function Argument (S : Switches_Source; Index : Positive) return String
     is abstract;

   type Command_Line_Switches_Source is
     limited new Switches_Source with null record;
   overriding function Argument_Count
     (S : Command_Line_Switches_Source) return Natural;
   overriding function Argument
     (S : Command_Line_Switches_Source; Index : Positive) return String;

   type String_List_Switches_Source (L : access String_List)  is
     limited new Switches_Source with null record;
   overriding function Argument_Count
     (S : String_List_Switches_Source) return Natural;
   overriding function Argument
     (S : String_List_Switches_Source; Index : Positive) return String;

end Switches;
