with Interfaces.C; use Interfaces.C;

with Support;

procedure Main is

   function Or_Else (L, R : int) return int;
   pragma Import (C, Or_Else, "or_else");

   function And_Then (L, R : Boolean) return Boolean is
   begin
      return L and then R;
   end And_Then;
begin
   Support.Assert (And_Then (True, True));
   Support.Assert (Or_Else (0, 1) = 1);
end Main;
