----------------------------------------------------------------------------
--                               LINKS (BODY)                             --
----------------------------------------------------------------------------

package body Links is

   -----------------------
   -- IOport operations --
   -----------------------

   procedure On_Push
     (Port : IOport_Access; Callback : Callback_Access) is
   begin
      Port.Push_Callback := Callback;
   end;

   procedure Push (Item : Data_Type; Port : IOport_Access) is
   begin
      Push (Item, Port.Data);
      if Port.Push_Callback /= null then
         Port.Push_Callback.all (Port);
      end if;
   end;

   procedure Pop (Item : out Data_Type; Port : IOport_Access) is
   begin
      Pop (Item, Port.Data);
   end;

   function Full (Port : IOport_Access) return Boolean is
   begin
      return Full (Port.Data);
   end;

   function Empty (Port : IOport_Access) return Boolean is
   begin
      return Empty (Port.Data);
   end;

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
   end;

   procedure Connect
     (Outp, Inp : IOport_Access; Link : IOlink_Access) is
   begin
      Inp.Link := Link;
      Outp.Link := Link;

      Link.Inp := Inp;
      Link.Outp := Outp;

      On_Push (Outp, Flush'Access);
   end;

end;

