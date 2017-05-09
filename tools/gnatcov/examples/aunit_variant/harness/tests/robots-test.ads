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

with Actors.Test;
with Robots_Devices;
with Geomaps;

package Robots.Test is

   type Test is new Actors.Test.Test with private;

   function Actor (T : Test) return Actor_Ref;

   procedure Set_Up (T : in out Test);

   procedure Test_Init (T : in out Test);
   procedure Test_Run (T : in out Test);

private

   type Test_Env is record
      Robot_Destroyed      : Boolean;
      Probe_Ahead_Square   : Geomaps.Square;
      Position             : Geomaps.Position;
      Direction            : Geomaps.Direction;
   end record;
   type Test_Env_Access is access all Test_Env;

   type Test is new Actors.Test.Test with record
      Test_Robot : Robot_Access := null;
      Hardware   : Robot_Hardware;
      Env        : Test_Env_Access;
   end record;

   type Test_Engine is new Robots_Devices.Engine with record
      Env : Test_Env_Access;
   end record;
   procedure Step_Forward (Device : access Test_Engine);
   procedure Rotate_Left  (Device : access Test_Engine);
   procedure Rotate_Right (Device : access Test_Engine);

   type Test_Radar is new Robots_Devices.Radar with record
      Env : Test_Env_Access;
   end record;
   function Probe_Ahead (Device : access Test_Radar) return Square;

   type Test_Locator is new Robots_Devices.Locator with record
      Env : Test_Env_Access;
   end record;
   function Get_Position  (Device : access Test_Locator) return Position;
   function Get_Direction (Device : access Test_Locator) return Direction;

end Robots.Test;
