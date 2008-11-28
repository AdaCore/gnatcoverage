----------------------------------------------------------------------------
--                                 EXPLORE                                --
----------------------------------------------------------------------------

--  This is the main unit for the Explore example.

-------------------------
-- Functional overview --
-------------------------

--  This example implements software pieces involved in the following
--  imaginary situation:
--
--  - A pilotless robot is set on a field to explore, where we expect to
--    find either regular ground or rock blocks.
--
--  - An inhabited station off the field communicates with the robot to
--    control it and visualize the explored portions of the field.

--  The robot is equipped with several devices:
--
--  - a steering engine, able to have the robot perform a short step
--    forward or rotate 90 deg either way without advancing,
--
--  - a front radar, able to probe the field characteristics (ground or
--    block) one step ahead,
--
--  - a locator, able to evaluate the robot's position and orientation on
--    the field.
--
--  The robot communicates with the station via two channels/links:
--
--  - a control link, through which the station sends commands for the
--    robot to execute (probe ahead, step forward, ...)
--
--  - a situation link, through which the robot sends info about it's
--    current situation (position, orientation, field ahead).
--
--  The field is modeled as a set of squares.  Below is a schematic
--  illustration of the various parts involved, with ...
--
--  'R' represting the robot (would actually be different characters for
--  each possible orientation), 'S' the station, '#' blocks and '?' squares
--  yet unexplored:
--
--            field                               view
--          ##########                          ??????????
--          #  #     #  <- control link         ?  #  ? ??
--          #    R<==========================>S ??   R  ??
--          #  #     #   situation link ->      ?  ?    ??
--          ##########                          ??????????
--
--  The Robot and Station active entities are both called Actors in this
--  world, and Links are attached to local Ports owned by these actors.
--

-----------------------------------
-- General software organization --
-----------------------------------

--  The set of units and essential abstractions involved is sketched below -
--  units underlined, associated abstractions parenthesized).
--
--  This set is a pretty straightforward mapping of the general concepts
--  involved, as described in the Functional Overview.  Only "Stacks" is a
--  pure support unit, offering the well known datastructure abstraction.
--
--  See the package specs for extra descriptive details.
--
--                              =======
--                              Explore
--                              =======
--
--   Geomaps (field Map, Situations + Links instance)
--   =======
--
--   Actors (Actor)         Robots (Robot)        Stations (Station)
--   ======                 ======                ========
--
--   Links (IOports, IOlinks)           Stacks (Stack)
--   =====                              ======
--
--   Controls (Robot_Control + Links instance)
--   ========

with Robots, Stations, Controls, Geomaps;
use Robots, Stations, Controls, Geomaps;

-------------
-- Explore --
-------------

--  This is the main subprogram for this example. It instanciates the
--  actors, the communication links, and schedules the actors runs in a very
--  simple cyclic fashion.
--
--  The field and the robot disposition there are implicitly created by the
--  Robot fake radar and locator devices, which both let it evaluate its
--  current situation.

procedure Explore is

   --  Instanciate the Robot and Station actors

   R : Robot_Access := new Robot;
   S : Station_Access := new Station;

begin
   Init (R);
   Init (S);

   --  Instantiate the Station/Robot communication Links and attach them
   --  to their dedicated ports on both sides.

   declare
      SR_Control_Link : Robot_Control_Links.IOlink_Access
        := new Robot_Control_Links.IOlink;
      SR_Situation_Link : Situation_Links.IOlink_Access
        := new Situation_Links.IOlink;
   begin
      Robot_Control_Links.Connect
        (Robot_Control_Outport(S.all), Robot_Control_Inport(R.all),
         SR_Control_Link);
      Situation_Links.Connect
        (Robot_Situation_Outport(R.all), Robot_Situation_Inport(S.all),
         SR_Situation_Link);
   end;

   --  Then run the cycles.

   while True loop
      Run (S);
      Run (R);
   end loop;
end;
