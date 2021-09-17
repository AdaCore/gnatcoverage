------------------------------------------------------------------------------
--                                                                          --
--                               GNATcoverage                               --
--                                                                          --
--                     Copyright (C) 2021, AdaCore                          --
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

with Ada.Containers.Ordered_Maps;

with GNAT.Strings;

with Strings; use Strings;

package Coverage_Options is

   type Coverage_Level is (Insn, Branch, Stmt, Decision, MCDC, UC_MCDC);
   --  Coverage objectives supported by xcov. The following values are
   --  supported:

   --  * object coverage at instruction level        (Insn);
   --  * object coverage at branch level             (Branch);
   --  * source coverage at statement level          (Stmt);
   --  * source coverage at decision level           (Decision);
   --  * source coverage at masking MC/DC level      (MCDC);
   --  * source coverage at unique cause MC/DC level (UC_MCDC).

   --  The terms "instruction", "branch", "statement", "decision" and "MCDC"
   --  should be understood here as they are defined in the DO-178B standard;
   --  their meaning is also documented in Couverture's documentation.

   subtype Object_Coverage_Level is Coverage_Level range Insn .. Branch;
   subtype Source_Coverage_Level is Coverage_Level range Stmt .. UC_MCDC;
   subtype MCDC_Coverage_Level   is Coverage_Level range MCDC .. UC_MCDC;

   type Levels_Type is array (Coverage_Level) of Boolean;
   --  Set of Coverage_Levels

   package Levels_Option_Maps is
     new Ada.Containers.Ordered_Maps
       (Key_Type     => GNAT.Strings.String_Access,
        Element_Type => Levels_Type,
        "<"          => Strings."<");
   Source_Levels_Option_Map : Levels_Option_Maps.Map;
   Object_Levels_Option_Map : Levels_Option_Maps.Map;

   function Coverage_Option_Value (L : Levels_Type) return String;
   --  Return the coverage option value corresponding to L

   function Source_Level_Options (Separator : String) return String;
   --  Return a string expression listing the valid --level alternatives
   --  for source coverage, separated by Separator.

   function Object_Level_Options (Separator : String) return String;
   --  Return a string expression describing valid --level alternatives
   --  for object coverage, separated by Separator.

end Coverage_Options;
