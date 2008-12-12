---------------------
--  QUEUES (BODY)  --
---------------------

package body Queues is

   -------------------------
   -- Classical accessors --
   -------------------------

   function Size (Q : Queue) return Natural is
   begin
      return Q.Size;
   end;

   function Full (Q : Queue) return Boolean is
   begin
      return Size (Q) = Q.Capacity;
   end;

   function Empty (Q : Queue) return Boolean is
   begin
      return Size (Q) = 0;
   end;

   procedure Pop (Item : out Data_Type; Q : in out Queue) is
   begin
      if Empty (Q) then
         raise Program_Error;
      end if;

      Item := Q.Items (Q.Front);
      if Q.Front = Q.Items'Last then
         Q.Front := Q.Items'First;
      else
         Q.Front := Q.Front + 1;
      end if;
      Q.Size := Q.Size - 1;
   end;

   procedure Push (Item : Data_Type; Q : in out Queue) is
   begin
      if Full (Q) then
         raise Program_Error;
      end if;

      Q.Items (Q.Back) := Item;
      if Q.Back = Q.Items'Last then
         Q.Back := Q.Items'First;
      else
         Q.Back := Q.Back + 1;
      end if;
      Q.Size := Q.Size + 1;
   end;

end;
