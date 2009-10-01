------------------------------------------------------------------------------
--                                                                          --
--                              Couverture                                  --
--                                                                          --
--                        Copyright (C) 2009, AdaCore                       --
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

package body Traces_Lines is

   procedure Update_Line_State (State : in out Line_State; El : Line_State) is
   begin
      case State is
         when No_Code =>
            State := El;
         when Not_Covered =>
            if El = Partially_Covered or else El = Covered then
               State := Partially_Covered;
            end if;
         when Partially_Covered =>
            null;
         when Covered =>
            if El = Not_Covered or else El = Partially_Covered then
               State := Partially_Covered;
            end if;
      end case;
   end Update_Line_State;
end Traces_Lines;
