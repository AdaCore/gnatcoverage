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

with AUnit.Test_Suites; use AUnit.Test_Suites;
with AUnit.Test_Caller;

generic

   Instance_Name : String;

package Links.Gen_Test.Gen_Suite is

   function Suite return Access_Test_Suite;

private

   package Caller is new AUnit.Test_Caller (Test);
   Test_Full_Access       : constant Caller.Test_Method := Test_Full'Access;
   Test_Empty_Access      : constant Caller.Test_Method := Test_Empty'Access;
   Test_Pop_Access        : constant Caller.Test_Method := Test_Pop'Access;
   Test_Push_Access       : constant Caller.Test_Method := Test_Push'Access;
   Test_Owner_Access      : constant Caller.Test_Method := Test_Owner'Access;
   Test_Connect_Access    : constant Caller.Test_Method := Test_Connect'Access;

   --  The following test exceptions raise. They require AUnit 3.2.
--     Test_Pop_Raise_Access  : constant Caller.Test_Method :=
--                                Test_Pop_Raise'Access;
--     Test_Push_Raise_Access : constant Caller.Test_Method :=
--                                Test_Push_Raise'Access;

end Links.Gen_Test.Gen_Suite;
