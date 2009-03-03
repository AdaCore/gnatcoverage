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
with Geomaps;          use Geomaps, Geomaps.Situation_Links;
with Controls;         use Controls, Controls.Robot_Control_Links;

package body Robots.Test is

   Test_Robot : aliased Robot;

   function New_Actor (T : Test) return Actor_Ref is
   begin
      Test_Robot :=
        (Actor with
         Robot_Control_Inp    => null,
         Robot_Situation_Outp => null,
         H                    => null,
         Mode                 => Cautious);

      return Test_Robot'Access;
   end New_Actor;

   ---------------
   -- Test_Init --
   ---------------

   procedure Test_Init (T : in out Test) is
   begin
      Init (Robot_Access (T.Act));

      Assert
        (Robot (T.Act.all).Robot_Control_Inp /= null,
         "Robot Control in port not initialized after call to init");

      Assert
        (Robot (T.Act.all).Robot_Situation_Outp /= null,
         "Robot Control out port not initialized after call to init");
   end Test_Init;

end Robots.Test;
