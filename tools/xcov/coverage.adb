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

with Strings; use Strings;

package body Coverage is

   Current_Action : Coverage_Action := Unknown_Coverage;
   --  Global variable that records the coverage operation
   --  that has been asked to xcov. This should be modified only
   --  one time by Set_Action.

   type Coverage_Option_Name_Array is
     array (Coverage_Action) of String_Acc;
   --  Table that maps a coverage action to its option strings.

   Coverage_Option_Names : constant Coverage_Option_Name_Array :=
     (Insn_Coverage     => new String'("insn"),
      Branch_Coverage   => new String'("branch"),
      Stmt_Coverage     => new String'("stmt"),
      Decision_Coverage => new String'("decision"),
      MCDC_Coverage     => new String'("mcdc"),
      Unknown_Coverage  => new String'("unknown"));

   function To_Coverage_Action (Option : String) return Coverage_Action is
   begin
      for Action in Known_Coverage_Action loop
         if Option = Coverage_Option_Names (Action).all then
            return Action;
         end if;
      end loop;
      return Unknown_Coverage;
   end To_Coverage_Action;

   function To_Coverage_Option (Action : Coverage_Action) return String is
   begin
      return Coverage_Option_Names (Action).all;
   end To_Coverage_Option;

   procedure Set_Action (Action : Coverage_Action) is
   begin
      pragma Assert (Current_Action = Unknown_Coverage);
      Current_Action := Action;
   end Set_Action;

   function Get_Action return Coverage_Action is
   begin
      return Current_Action;
   end Get_Action;

   procedure Dump_Coverage_Option (Report : File_Access) is
   begin
      Put_Line (Report.all,
                "Coverage level: " & To_Coverage_Option (Current_Action));
   end Dump_Coverage_Option;

end Coverage;
