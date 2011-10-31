------------------------------------------------------------------------------
--                                                                          --
--                              Couverture                                  --
--                                                                          --
--                      Copyright (C) 2008-2009, AdaCore                    --
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

--  This unit exposes the Robot command/control enumerals and the associated
--  Link abstraction for the Explore example.

with Links;

package Controls is

   --  Possible Robot Operational modes

   type Robot_Opmode is
     (Cautious,  --  The Robot won't execute potentially damaging commands
      Dumb       --  The Robot executes all the commands it receives
     );

   --  Commands the robot understands on its "robot control" input port.

   type Robot_Command is
     (Nop,           --  Do nothing
      Opmode,        --  Switch to new operational mode
      Step_Forward,  --  Step forward
      Rotate_Right,  --  Rotate 90 deg, counterclockwise
      Rotate_Left,   --  Rotate 90 deg, clockwise
      Probe          --  Probe field one step ahead and report
     );

   type Robot_Control is record
      Code  : Robot_Command;
      Value : Integer; --  Possible argument associated with command CODE.
   end record;

   package Robot_Control_Links is new Links (Data_Type => Robot_Control);
end Controls;
