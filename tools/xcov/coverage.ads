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

with Ada.Text_IO; use Ada.Text_IO;

with Traces_Dbase; use Traces_Dbase;
with Traces_Elf;   use Traces_Elf;

package Coverage is

   type Coverage_Level is (Insn, Branch, Stmt, Decision, MCDC, Unknown);
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

   function To_Coverage_Level (Option : String) return Coverage_Level;
   --  Option being the name of a coverage Level (i.e. "insn", "branch",
   --  "stmt", "decision", "mcdc"), return the corresponding
   --  Known_Coverage_Level, or Unknown if none of them matches.

   function To_Coverage_Option (Level : Coverage_Level) return String;
   --  Return the name of the coverage Level given in parameter

   procedure Set_Coverage_Level (Level : Coverage_Level);
   --  Set the coverage Level to be perform by xcov. As xcov only support
   --  one coverage Level per run, this shall only be called once.

   function Get_Coverage_Level return Coverage_Level;
   pragma Inline (Get_Coverage_Level);
   --  Return the current coverage Level

   procedure Dump_Coverage_Option (Report : File_Access);
   --  Dump the coverage option information to Report

   function All_Known_Coverage_Levels return String;
   --  Return |-separated list of all values in Known_Coverage_Level (for
   --  help message).

end Coverage;
