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

package body Links.Gen_Test.Gen_Suite is

   function Suite return Access_Test_Suite is
      Ret : Access_Test_Suite := New_Suite;
   begin
      AUnit.Test_Suites.Add_Test
        (Ret, Caller.Create
          ("Test " & Instance_Name & ".Full", Test_Full_Access));
      AUnit.Test_Suites.Add_Test
        (Ret, Caller.Create
          ("Test " & Instance_Name & ".Empty", Test_Empty_Access));
      AUnit.Test_Suites.Add_Test
        (Ret, Caller.Create
          ("Test " & Instance_Name & ".Pop", Test_Pop_Access));
      AUnit.Test_Suites.Add_Test
        (Ret, Caller.Create
          ("Test " & Instance_Name & ".Push", Test_Push_Access));
      AUnit.Test_Suites.Add_Test
        (Ret, Caller.Create
          ("Test " & Instance_Name & ".Owner", Test_Owner_Access));
      AUnit.Test_Suites.Add_Test
        (Ret, Caller.Create
           ("Test " & Instance_Name & ".Connect", Test_Connect_Access));

--        AUnit.Test_Suites.Add_Test
--          (Ret, Caller.Create
--             ("Test " & Instance_Name & ".Pop raises expected exceptions",
--              Test_Pop_Raise_Access));
--        AUnit.Test_Suites.Add_Test
--          (Ret, Caller.Create
--             ("Test " & Instance_Name & ".Push raises expected exceptions",
--              Test_Push_Raise_Access));

      return Ret;
   end Suite;

end Links.Gen_Test.Gen_Suite;
