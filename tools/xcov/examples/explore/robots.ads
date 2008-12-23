----------------------------------------------------------------------------
--                              ROBOTS (SPEC)                             --
----------------------------------------------------------------------------

--  This unit exposes the Robot abstraction of the Explore example.
--
--  A Robot is an active entity able to execute Robot_Control commands
--  received through an input Control port. Placed on a field, the Robot is
--  able to evaluate its current Situation (location, orientation, field
--  square ahead), and send this through an output Situation port.
--
--  A Robot operates in one of several modes, as described in the Controls
--  abstraction.

with Actors, Geomaps, Controls;
use Actors, Geomaps, Controls;

package Robots is

   type Robot is new Actor with private;
   type Robot_Access is access all Robot;

   function Robot_Control_Inport
     (R : Robot) return Robot_Control_Links.IOport_Access;
   function Robot_Situation_Outport
     (R : Robot) return Situation_Links.IOport_Access;

   procedure Init (R : Robot_Access);
   --  Initialize robot R - setup ports and internal devices

   procedure Run (R : Robot_Access);
   --  Run robot R - process pending messages on input ports

   type Robot_Opmode is (Cautious, Dumb);

private

   type Robot_Hardware;
   type Robot_Hardware_Access is access Robot_Hardware;

   type Robot is new Actor with record
      Robot_Control_Inp : Robot_Control_Links.IOport_Access;
      Robot_Situation_Outp : Situation_Links.IOport_Access;

      H : Robot_Hardware_Access;
      Mode : Robot_Opmode := Cautious;
   end record;
end;


