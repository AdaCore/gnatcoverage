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

   ---------
   -- "*" --
   ---------

   function "*" (L, R : Line_State) return Line_State is
   begin
      if R = No_Code then
         return L;
      else
         case L is
            when No_Code =>
               return R;

            when Not_Covered =>
               return Line_State'Min (R, Partially_Covered);

            when Partially_Covered =>
               return L;

            when Covered =>
               return Line_State'Max (R, Partially_Covered);
         end case;
      end if;
   end "*";

end Traces_Lines;
