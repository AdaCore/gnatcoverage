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

--  This unit exposes the Robot abstraction of the Explore example.
--
--  A Robot is an active entity able to execute Robot_Control commands
--  received through an input Control port. Placed on a field, the Robot is
--  able to evaluate its current Situation (location, orientation, field
--  square ahead), and send this through an output Situation port.
--
--  A Robot operates in one of several modes, as described in the Controls
--  abstraction.

with Actors, Geomaps, Controls, Robots_Devices;
use Actors, Geomaps, Controls, Robots_Devices;

package Robots is

   type Robot is new Actor with private;
   type Robot_Access is access all Robot;

   function Robot_Control_Inport
     (R : Robot) return Robot_Control_Links.IOport_Access;
   function Robot_Situation_Outport
     (R : Robot) return Situation_Links.IOport_Access;

   procedure Init (R : Robot_Access; Hw : Robot_Hardware);
   --  Initialize robot R using the hardware Hw - setup ports and internal
   --  devices

   procedure Run (R : Robot_Access);
   --  Run robot R - process pending messages on input ports

   type Robot_Opmode is (Cautious, Dumb);

private

   type Robot is new Actor with record
      Robot_Control_Inp    : Robot_Control_Links.IOport_Access;
      Robot_Situation_Outp : Situation_Links.IOport_Access;

      Hw                   : Robot_Hardware;
      Mode                 : Robot_Opmode := Cautious;
   end record;

   function Unsafe (Cmd : Robot_Command; Sqa : Square) return Boolean;
   --  Whether execution of CMD is unsafe with the SQA square ahead

end Robots;
