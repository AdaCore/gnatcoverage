----------------------------------------------------------------------------
--                               STACKS (BODY)                            --
----------------------------------------------------------------------------

package body Stacks is

   function Size (S : Stack) return Natural;
   pragma Inline (Size);
   --  Number of elements currently available in S

   function Size (S : Stack) return Natural is
   begin
      return S.Top_In;
   end;

   function Full (S : Stack) return Boolean is
   begin
      return Size (S) = S.Capacity;
   end;

   function Empty (S : Stack) return Boolean is
   begin
      return Size (S) = 0;
   end;

   procedure Pop (Item : out Data_Type; S : in out Stack) is
   begin
      if Empty (S) then
         raise Program_Error;
      end if;
      Item := S.Items (S.Top_In);
      S.Top_In := S.Top_In - 1;
   end;

   procedure Push (Item : Data_Type; S : in out Stack) is
   begin
      if Full (S) then
         raise Program_Error;
      end if;
      S.Top_In := S.Top_In + 1;
      S.Items (S.Top_In) := Item;
   end;

end;
