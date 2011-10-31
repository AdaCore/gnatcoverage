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

package body Links is

   -----------------------
   -- IOport operations --
   -----------------------

   function Create_IOport
     (Capacity : Natural; Owner : Actor_Ref) return IOport_Access
   is
      Ret : IOport_Access := new IOport (Capacity);
   begin
      Ret.Owner := Owner;
      return Ret;
   end Create_IOport;

   procedure On_Push
     (Port : IOport_Access; Callback : Callback_Access) is
   begin
      Port.Push_Callback := Callback;
   end On_Push;

   procedure Push (Item : Data_Type; Port : IOport_Access) is
   begin
      Push (Item, Port.Data);
      if Port.Push_Callback /= null then
         Port.Push_Callback.all (Port);
      end if;
   end Push;

   procedure Pop (Item : out Data_Type; Port : IOport_Access) is
   begin
      Pop (Item, Port.Data);
   end Pop;

   function Full (Port : IOport_Access) return Boolean is
   begin
      return Full (Port.Data);
   end Full;

   function Empty (Port : IOport_Access) return Boolean is
   begin
      return Empty (Port.Data);
   end Empty;

   function Owner (Port : IOport_Access) return Actor_Ref is
   begin
      return Port.Owner;
   end Owner;

   ------------------------
   -- IOlinks operations --
   ------------------------

   procedure Flush (Port : IOport_Access);
   --  Flush all the data available from Port through the Link to which it
   --  is connected.  This is called when data is pushed into Port after it
   --  has been Connected (linked) to another one.

   procedure Flush (Port : IOport_Access) is
      Link : IOlink_Access := Port.Link;
      Data : Data_Type;
   begin
      while not Empty (Link.Outp) loop
         Pop (Data, Link.Outp);
         Push (Data, Link.Inp);
      end loop;
   end Flush;

   procedure Connect
     (Outp, Inp : IOport_Access; Link : IOlink_Access) is
   begin
      Inp.Link := Link;
      Outp.Link := Link;

      Link.Inp := Inp;
      Link.Outp := Outp;

      On_Push (Outp, Flush'Access);
   end Connect;

end Links;
