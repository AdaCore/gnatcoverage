------------------------------------------------------------------------------
--                                                                          --
--                               GNATcoverage                               --
--                                                                          --
--                     Copyright (C) 2009-2012, AdaCore                     --
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

--  This package provides primitives to handle coverage Levels that can be
--  performed by xcov: how to parse them from the command line and how to print
--  them into reports.
--  This package also keeps the track of the coverage Level selected
--  for one run; this Level will be unique and will not change after
--  it has been initialized.

with Traces_Dbase; use Traces_Dbase;
with Traces_Elf;   use Traces_Elf;

package Coverage is

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
   --  should be undertstood here as they are defined in the DO-178B standard;
   --  their meaning is also documented in Couverture's documentation.

   subtype Object_Coverage_Level is Coverage_Level range Insn .. Branch;
   subtype Source_Coverage_Level is Coverage_Level range Stmt .. UC_MCDC;
   subtype MCDC_Coverage_Level   is Coverage_Level range MCDC .. UC_MCDC;

   procedure Set_Coverage_Levels (Opt : String);
   --  Set the coverage levels to be assessed by xcov

   function Valid_Coverage_Options return String;
   --  Return |-separated list of acceptable values for Set_Coverage_Levels

   function Enabled (Level : Coverage_Level) return Boolean;
   --  True when Level is enabled

   function Object_Coverage_Enabled return Boolean;
   --  True when any Object_Coverage_Level is enabled

   function Source_Coverage_Enabled return Boolean;
   --  True if any Source_Coverage_Level is enabled

   function MCDC_Coverage_Enabled return Boolean;
   --  True if any MCDC_Coverage_Level is enabled

   function Object_Level return Object_Coverage_Level;
   --  If Object_Coverage_Enabled, return Insn or Branch, depending on which
   --  one is enabled (it is illegal to have both enabled).

   function MCDC_Level return MCDC_Coverage_Level;
   --  If MCDC_Coverage_Level_Enabled, return MCDC or UC_MCDC, depending on
   --  which one is enabled (it is illegal to have both enabled).

   function Coverage_Option_Value return String;
   --  Return the coverage option value for the currently enabled levels

end Coverage;
