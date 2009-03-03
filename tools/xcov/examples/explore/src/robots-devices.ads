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

private package Robots.Devices is

   type Hardware_Access is private;

   procedure Init (H : out Hardware_Access);
   --  Initialize a Robot devices structure and have H designate it

   procedure Probe (Situ : out Situation; H : Hardware_Access);
   --  Use the devices to fill SITU with the current Situation

   function Safe (Ctrl : Robot_Control; H : Hardware_Access) return Boolean;
   --  Whether execution of CTRL by the devices is safe.
   --  CTRL is safe when the square ahead is neither a rock block nor water, or
   --  CTRL is not a "step forward" command.

   procedure Execute (Ctrl : Robot_Control; H : Hardware_Access);
   --  Use the devices to execute CTRL

private

   --  There will be fake implementations of this unit for pure software
   --  simulation purposes, so we defer the type definition to the package
   --  body.

   type Hardware;
   type Hardware_Access is access Hardware;
end Robots.Devices;
