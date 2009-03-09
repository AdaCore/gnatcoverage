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
--  This package provides primitives to handle coverage actions that can
--  be performed by xcov; roughly, how to parse them from the command line
--  and how to print them into reports.
--  This package also keeps the track of the coverage action selected
--  for one run; this action will be unique and will not change after
--  it has been initialized.

with Ada.Text_IO; use Ada.Text_IO;

package Coverage is

   type Coverage_Action is (Insn_Coverage, Branch_Coverage, Stmt_Coverage,
                            Decision_Coverage, MCDC_Coverage,
                            Unknown_Coverage);
   --  Coverage actions supported by xcov, plus "unknown" (to be used when
   --  no coverage action has been specified). The following subtypes
   --  defines different subclasses. So, this includes:
   --  * object coverage at instruction level (Insn_Coverage);
   --  * object coverage at branch level      (Branch_Coverage);
   --  * source coverage at statement level   (Stmt_Coverage);
   --  * source coverage at decision level    (Decision_Coverage);
   --  * source coverage at MCDC level        (MCDC_Coverage).
   --  the terms "instruction", "branch", "statement", "decision"
   --  and "MCDC" should be undertstood here as they are defined in
   --  the DO-178B standard; their meaning is also documented in
   --  Couverture's documentation.

   subtype Known_Coverage_Action is
     Coverage_Action range Insn_Coverage .. MCDC_Coverage;

   subtype Object_Coverage_Action is
     Known_Coverage_Action range Insn_Coverage .. Branch_Coverage;

   subtype Source_Coverage_Action is
     Known_Coverage_Action range Stmt_Coverage .. MCDC_Coverage;

   Coverage_Option       : constant String := "--coverage=";
   Coverage_Option_Short : constant String := "-c";
   --  xcov's command line options for coverage. The parameter of
   --  this option is valid iff To_Coverage_Action can convert it to
   --  a Known_Coverage_Action.

   function To_Coverage_Action (Option : String) return Coverage_Action;
   --  Option being the name of a coverage action (i.e. "insn", "branch",
   --  "stmt", "decision", "mcdc"), return the corresponding
   --  Known_Coverage_Action, or Unknown_Coverage if none of them matches.

   function To_Coverage_Option (Action : Coverage_Action) return String;
   --  Return the name of the coverage action given in parameter.

   procedure Set_Action (Action : Coverage_Action);
   --  Set the coverage action to be perform by xcov. As xcov only support
   --  one coverage action per run, this shall only be called once.

   function Get_Action return Coverage_Action;
   pragma Inline (Get_Action);
   --  Return the current coverage action.

   procedure Dump_Coverage_Option (Report : File_Access);
   --  Dump the coverage option information to Report.

end Coverage;
