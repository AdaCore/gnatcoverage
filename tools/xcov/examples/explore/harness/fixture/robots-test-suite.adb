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

package body Robots.Test.Suite is

   -----------
   -- Suite --
   -----------

   function Suite return AUnit.Test_Suites.Access_Test_Suite is
      Ret : constant Access_Test_Suite := New_Suite;
   begin
      AUnit.Test_Suites.Add_Test
        (Ret, Caller.Create
           ("Test Robots.Kill/Live", Test_Live_And_Kill_Access));
      AUnit.Test_Suites.Add_Test
        (Ret, Caller.Create
           ("Test Robots.Init", Test_Init_Access));
      AUnit.Test_Suites.Add_Test
        (Ret, Caller.Create
           ("Test Robots.Run", Test_Run_Access));
      return Ret;
   end Suite;

end Robots.Test.Suite;
