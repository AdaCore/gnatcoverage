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

package body Actors.Test is

   procedure Set_Up (T : in out Test) is
   begin
      T.Act := New_Actor (Test'Class (T));
   end Set_Up;

   ---------------
   -- Test_Live --
   ---------------

   procedure Test_Live_And_Kill (T : in out Test) is
   begin
      Assert (Live (T.Act.all), "Actor should be alived by default");
      Kill (T.Act.all);
      Assert (not Live (T.Act.all), "Actor should not be alived after a kill");
   end Test_Live_And_Kill;

end Actors.Test;
