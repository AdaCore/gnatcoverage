------------------------------------------------------------------------------
--                                                                          --
--                              Couverture                                  --
--                                                                          --
--                     Copyright (C) 2008-2009, AdaCore                     --
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

with Coverage; use Coverage;

package body Traces_Lines is

   -----------------------
   -- Update_Line_State --
   -----------------------

   procedure Update_Line_State (L : in out Line_State;
                                I : Known_Insn_State) is
   begin
      pragma Assert (Get_Coverage_Level = Insn
                       or else Get_Coverage_Level = Branch);
      --  This assertion is justified in Update_Line_State's specification

      case L is
         when Not_Covered =>
            if I = Not_Covered then
               L := Not_Covered;
            else
               L := Partially_Covered;
            end if;

         when Partially_Covered =>
            null;

         when Covered =>
            if I = Covered or else I = Both_Taken then
               L := Covered;
            else
               L := Partially_Covered;
            end if;

         when No_Code =>
            if I = Covered or else I = Both_Taken then
               L := Covered;
            elsif I = Not_Covered then
               L := Not_Covered;
            else
               L := Partially_Covered;
            end if;

      end case;
   end Update_Line_State;

end Traces_Lines;
