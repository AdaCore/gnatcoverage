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

--  This unit exposes the Control Station abstraction of the Explore
--  example.  The station gets user input about the next robot order to
--  transmit, sends them over the control link, receives the situation
--  report over the situation link and maintains a local view of the field
--  as it gets explored.

with Actors, Controls, Geomaps; use Actors, Controls, Geomaps;

package Stations is

   type Station is new Actor with private;
   type Station_Access is access all Station;

   function Robot_Control_Outport
     (Sta : Station) return Robot_Control_Links.IOport_Access;
   function Robot_Situation_Inport
     (Sta : Station) return Situation_Links.IOport_Access;

   procedure Init (Sta : Station_Access);
   --  Initialize station STA - setup ports and local field view

   procedure Run (Sta : Station_Access);
   --  Run a single command processing cycle on station STA

private
   type Station is new Actor with record
      Robot_Control_Outp : Robot_Control_Links.IOport_Access;
      Robot_Situation_Inp : Situation_Links.IOport_Access;

      Map : Geomap;
   end record;
end Stations;
