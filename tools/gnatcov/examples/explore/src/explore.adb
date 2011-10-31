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

--  This is the main unit for the Explore example, a sample application used
--  to introduce/illustrate a number of concepts in the Xcov documentation.
--
--  This example features a fake robot exploring a field, controled through
--  communication channels by a inhabited control station off the field. See
--  the Overview dummy spec for a functional and organisational overview,
--  separated out to allow inclusion into the documentation.

with Overview, Actors, Robots, Stations, Controls, Geomaps;
use  Overview, Actors, Robots, Stations, Controls, Geomaps;

with Robots_Devices.Dummy;

-------------
-- Explore --
-------------

--  This is the main subprogram for this example. It instanciates the
--  actors, the communication links, and schedules the actors runs in a
--  simple cyclic fashion.
--
--  The field and the robot disposition are implicitly created by the
--  Robot fake radar and locator devices, which both let it evaluate its
--  current situation.

procedure Explore is

   --  Instanciate the Robot and Station actors

   R : Robot_Access := new Robot;
   S : Station_Access := new Station;

begin
   Init (R, Robots_Devices.Dummy.Get_Hardware);
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
        (Robot_Control_Outport (S.all), Robot_Control_Inport (R.all),
         SR_Control_Link);
      Situation_Links.Connect
        (Robot_Situation_Outport (R.all), Robot_Situation_Inport (S.all),
         SR_Situation_Link);
   end;

   --  Then run the cycles until one of the actors dies.

   while True loop
      Run (S);
      exit when not Live (S.all);
      Run (R);
      exit when not Live (R.all);
   end loop;
end Explore;
