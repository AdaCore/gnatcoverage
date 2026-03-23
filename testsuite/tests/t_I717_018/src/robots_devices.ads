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

--  This unit offers a simple device access internal API to the Robot
--  abstraction of the Explore example.
--
--  This API abstracts the capabilities of
--
--  - a steering engine, able to have the robot perform a short step
--    forward or rotate 90 deg either way without advancing,
--
--  - a front radar, able to probe the field characteristics one step ahead,
--
--  - a locator, able to evaluate the robot's position and orientation on
--    the field.

with Controls, Geomaps;
use Controls, Geomaps;

package Robots_Devices is

   type Engine is abstract tagged null record;
   type Engine_Access is access all Engine'Class;

   procedure Step_Forward (Device : access Engine) is abstract;
   --  Make the Engines step forward

   procedure Rotate_Left    (Device : access Engine) is abstract;
   --  Make the Engines rotate left

   procedure Rotate_Right   (Device : access Engine) is abstract;
   --  Make the Engines rotate right

   type Radar is abstract tagged null record;
   type Radar_Access is access all Radar'Class;

   function Probe_Ahead (Device : access Radar) return Square is abstract;
   --  Use the radar to determine what kind of square is ahead

   type Locator is abstract tagged null record;
   type Locator_Access is access all Locator'Class;

   function Get_Position
     (Device : access Locator) return Position is abstract;
   function Get_Direction
     (Device : access Locator) return Direction is abstract;
   --  Use the locator to retrieve the current position and direction.

   type Robot_Hardware is record
      Eng : Engine_Access;
      Rad : Radar_Access;
      Loc : Locator_Access;
   end record;

end Robots_Devices;
