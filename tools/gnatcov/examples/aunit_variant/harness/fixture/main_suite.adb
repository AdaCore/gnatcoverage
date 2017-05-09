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

with Controls.Test.Suite;
with Geomaps.Test.Suite;
with Robots.Test.Suite;
with Stations.Test.Suite;

package body Main_Suite is

   function Suite return Access_Test_Suite is
      Ret : constant Access_Test_Suite := New_Suite;
   begin
      AUnit.Test_Suites.Add_Test
        (Ret,
         Controls.Test.Suite.Robot_Control_Link_Suite.Suite);
      AUnit.Test_Suites.Add_Test
        (Ret,
         Geomaps.Test.Suite.Suite);
      AUnit.Test_Suites.Add_Test
        (Ret,
         Robots.Test.Suite.Suite);
      AUnit.Test_Suites.Add_Test
        (Ret,
         Stations.Test.Suite.Suite);
      return Ret;
   end Suite;

end Main_Suite;
