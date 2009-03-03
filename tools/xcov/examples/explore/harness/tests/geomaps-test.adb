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

with AUnit.Assertions; use AUnit.Assertions;

package body Geomaps.Test is

   -----------------------
   -- Test_Pos_Ahead_Of --
   -----------------------

   procedure Test_Pos_Ahead_Of (T : in out Test)
   is
      Situ : Situation :=
               (Pos => (X => 3, Y => 3),
                Dir => South,
                Sqa => Ground);
      Res  : Position;

   begin
      Res := Pos_Ahead_Of (Situ);
      Assert
        (Res.X = 3 and then Res.Y = 4,
         "Pos_Ahead_Of returns incorrect value when going South");

      Situ.Dir := North;
      Res := Pos_Ahead_Of (Situ);
      Assert
        (Res.X = 3 and then Res.Y = 2,
         "Pos_Ahead_Of returns incorrect value when going North");

      Situ.Dir := East;
      Res := Pos_Ahead_Of (Situ);
      Assert
        (Res.X = 4 and then Res.Y = 3,
         "Pos_Ahead_Of returns incorrect value when going East");

      Situ.Dir := West;
      Res := Pos_Ahead_Of (Situ);
      Assert
        (Res.X = 2 and then Res.Y = 3,
         "Pos_Ahead_Of returns incorrect value when going West");
   end Test_Pos_Ahead_Of;

end Geomaps.Test;
