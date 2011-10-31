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

with AUnit.Test_Caller;
with AUnit.Test_Suites; use AUnit.Test_Suites;

package Robots.Test.Suite is

   function Suite return AUnit.Test_Suites.Access_Test_Suite;

private

   package Caller is new AUnit.Test_Caller (Robots.Test.Test);
   Test_Live_And_Kill_Access : constant Caller.Test_Method :=
                                 Test_Live_And_Kill'Access;
   Test_Init_Access          : constant Caller.Test_Method :=
                                 Test_Init'Access;
   Test_Run_Access           : constant Caller.Test_Method :=
                                 Test_Run'Access;

end Robots.Test.Suite;
