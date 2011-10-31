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

pragma Ada_2005;

with AUnit.Test_Fixtures;

with Actors;

generic

   Act1, Act2 : Actors.Actor_Ref;
   Data_Val1  : Links.Data_Type;
   Data_Val2  : Links.Data_Type;

package Links.Gen_Test is

   type Test is new AUnit.Test_Fixtures.Test_Fixture with private;

   procedure Set_Up (T : in out Test);

   procedure Test_Full    (T : in out Test);
   procedure Test_Empty   (T : in out Test);
   procedure Test_Pop     (T : in out Test);
   procedure Test_Push    (T : in out Test);
   procedure Test_Owner   (T : in out Test);
   procedure Test_Connect (T : in out Test);

   --  The following tests require AUnit 3.2.
   --  Uncomment also in links-gen_test-gen_suite.ad[bs] to enable
--     procedure Test_Pop_Raise  (T : in out Test);
--     procedure Test_Push_Raise (T : in out Test);

private

   type Test is new AUnit.Test_Fixtures.Test_Fixture with record
      Port0 : Links.IOport_Access := Links.Create_IOport (0, null);
      Port1 : Links.IOport_Access := Links.Create_IOport (1, null);
      Port4 : Links.IOport_Access := Links.Create_IOport (4, null);
      Link  : Links.IOlink_Access := new Links.IOlink;
      Inp   : Links.IOport_Access := Links.Create_IOport (1, Act1);
      Outp  : Links.IOport_Access := Links.Create_IOport (1, Act2);
   end record;

end Links.Gen_Test;
