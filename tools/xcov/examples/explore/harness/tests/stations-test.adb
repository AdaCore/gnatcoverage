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

package body Stations.Test is

   Test_Station : aliased Station;

   function New_Actor (T : Test) return Actor_Ref is
   begin
      Test_Station :=
        (Actor with
         Robot_Control_Outp  => null,
         Robot_Situation_Inp => null,
         Map                 => (others => (others => Ground)));

      return Test_Station'Access;
   end New_Actor;

   ---------------
   -- Test_Init --
   ---------------

   procedure Test_Init (T : in out Test) is
   begin
      Init (Station_Access (T.Act));

      Assert
        (Station (T.Act.all).Robot_Control_Outp /= null,
         "Robot Constrol out port not initialized after call to init");

      Assert
        (Station (T.Act.all).Robot_Situation_Inp /= null,
         "Robot Constrol out port not initialized after call to init");
   end Test_Init;

end Stations.Test;
