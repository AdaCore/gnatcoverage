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

--  This package provides primitives to handle coverage Levels that can be
--  performed by xcov: how to parse them from the command line and how to print
--  them into reports.
--  This package also keeps the track of the coverage Level selected
--  for one run; this Level will be unique and will not change after
--  it has been initialized.

with Traces_Dbase; use Traces_Dbase;
with Traces_Elf;   use Traces_Elf;

package Coverage is

   type Coverage_Level is (Insn, Branch, Stmt, Decision, MCDC);
   --  Coverage objectives supported by xcov, plus "unknown" (to be used when
   --  no coverage objective has been specified). The following values are
   --  supported:

   --  * object coverage at instruction level (Insn);
   --  * object coverage at branch level      (Branch);
   --  * source coverage at statement level   (Stmt);
   --  * source coverage at decision level    (Decision);
   --  * source coverage at MCDC level        (MCDC).

   --  The terms "instruction", "branch", "statement", "decision" and "MCDC"
   --  should be undertstood here as they are defined in the DO-178B standard;
   --  their meaning is also documented in Couverture's documentation.

   subtype Known_Coverage_Level  is Coverage_Level range Insn .. MCDC;
   subtype Object_Coverage_Level is Coverage_Level range Insn .. Branch;
   subtype Source_Coverage_Level is Coverage_Level range Stmt .. MCDC;

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

   function Coverage_Option_Value return String;
   --  Return the coverage option value for the currently enabled levels

end Coverage;
