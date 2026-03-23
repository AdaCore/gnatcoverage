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

--  Fake implementation of robots devices, using a static Map.

package Robots_Devices.Dummy is

   type Dummy_Engine is new Robots_Devices.Engine with private;

   procedure Step_Forward (Device : access Dummy_Engine);
   --  Make the Engines step forward

   procedure Rotate_Left    (Device : access Dummy_Engine);
   --  Make the Engines rotate left

   procedure Rotate_Right   (Device : access Dummy_Engine);
   --  Make the Engines rotate right

   type Dummy_Radar is new Robots_Devices.Radar with private;

   function Probe_Ahead (Device : access Dummy_Radar) return Square;
   --  Use the radar to determine what kind of square is ahead

   type Dummy_Locator is new Robots_Devices.Locator with private;

   function Get_Position  (Device : access Dummy_Locator) return Position;
   function Get_Direction (Device : access Dummy_Locator) return Direction;
   --  Use the locator to retrieve the current position and direction.

   function Get_Hardware return Robot_Hardware;
   --  Get a complete hardware set based on the above devices.

private

   type Environment is record
      Map  : Geomap;
      Situ : Situation;
   end record;

   type Environment_Access is access Environment;
   --  Shared environment for all devices.

   type Dummy_Engine is new Robots_Devices.Engine with record
      Env : Environment_Access;
   end record;

   type Dummy_Radar is new Robots_Devices.Radar with record
      Env : Environment_Access;
   end record;

   type Dummy_Locator is new Robots_Devices.Locator with record
      Env : Environment_Access;
   end record;

end Robots_Devices.Dummy;
