with Ada.Text_IO; use Ada.Text_IO;

with Interfaces.C; use Interfaces.C;

with Pkg;

procedure Test is
   function And_Then (A, B : Boolean) return Boolean;

   function And_Then (A, B : Boolean) return Boolean is
   begin
      return A and then B;
   end And_Then;

begin
   Put_Line (int'Image (Pkg.And_Then (0, 1)));
   Put_Line (Boolean'Image (And_Then (Pkg.Id (True), False)));
end Test;
