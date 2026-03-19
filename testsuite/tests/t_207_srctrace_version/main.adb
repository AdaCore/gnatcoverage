with Interfaces.C; use Interfaces.C;

procedure Main is


   function Or_Else (L, R : int) return int;
   pragma Import (C, Or_Else, "or_else");

   procedure Assert (B : Boolean) is
   begin
      if not B then
         raise Program_Error;
      end if;
   end Assert;

   function And_Then (L, R : Boolean) return Boolean is
   begin
      return L and then R;
   end And_Then;
begin
   Assert (And_Then (True, True));
   Assert (Or_Else (0, 1) = 1);
end Main;
