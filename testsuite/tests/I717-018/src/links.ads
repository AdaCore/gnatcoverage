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

--  This package exposes IO Ports and Links abstractions to represent
--  interconnections between Actors.

--  A Port is an abstract interface point into which data may be pushed
--  or out of which data may be popped, possibly buffered up to a specified
--  Capacity. A Link is an abstract communication channel which connects an
--  output Port to an input Port, all for a specific data type.

--  Ports are meant to be attached to (owned by) an Actor to let it either
--  receive data (input ports) or emit data (output ports), not both.
--
--  Links are meant to allow interconnections between actors via the ports
--  they own.  The simple figure below illustrates such an interconnection
--  between
--
--    a source actor Sa with an output port Op, and
--    a destination actor Da with an input port Ip
--
--         +======+                    +======+
--         |  Da  Ip <---- Link ----< Op  Sa  |
--         +======+                    +======+
--
--  Operation-wise, Links operate as data push propagators. In the example
--  above, data Pushed by Sa into Op becomes available for a Pop out of Ip
--  by Da at the other end of the Link.
--
--  At this stage, a port may connect to at most one link and links support
--  only one to one connections.

with Actors, Queues;
use  Actors;

generic
   type Data_Type is private;
package Links is

   ------------
   -- IOport --
   ------------

   type IOport (<>) is private;
   type IOport_Access is access IOport;

   function Create_IOport
     (Capacity : Natural; Owner : Actor_Ref) return IOport_Access;
   --  The IO Port constructor.

   function Full (Port : IOport_Access) return Boolean;
   --  Whether Port is full with respect to its Capacity.

   function Empty (Port : IOport_Access) return Boolean;
   --  Whether Port is empty.

   procedure Push (Item : Data_Type; Port : IOport_Access);
   --  Push Item on top into Port if it is not Full,
   --  raise Program_Error and leave Port unchanged otherwise.

   procedure Pop (Item : out Data_Type; Port : IOport_Access);
   --  Pop the oldest data Item out of Port if it is not Empty,
   --  raise Program_Error and leave Item undefined otherwise.

   function Owner (Port : IOport_Access) return Actor_Ref;
   --  Retrieve the owner of the port.

   ------------
   -- IOlink --
   ------------

   type IOlink is private;
   type IOlink_Access is access IOlink;

   procedure Connect (Outp, Inp : IOport_Access; Link : IOlink_Access);
   --  Connect the output port Outp to the input port Inp via Link.  Data
   --  pushed into Outp is immediately popped out of it and pushed into Inp.

private

   --  While they are intended to represent almost concrete concepts, ports
   --  as we expose them much resemble queues, except for the support of a
   --  callback on push.

   package Data_Queue_P is new Queues (Data_Type => Data_Type);
   use Data_Queue_P;

   subtype Data_Queue is Data_Queue_P.Queue;

   type Callback_Access is access procedure (Port : IOport_Access);

   type IOport (Capacity : Natural) is record
      Data  : Data_Queue (Capacity => Capacity);
      Push_Callback : Callback_Access;

      Owner : Actor_Ref;
      Link  : IOlink_Access;
   end record;

   procedure On_Push (Port : IOport_Access; Callback : Callback_Access);
   --  Register Callback as a subprogram to be called every time data is
   --  pushed into Port.

   type IOlink is record
      Inp, Outp : IOport_Access;
   end record;

end Links;
