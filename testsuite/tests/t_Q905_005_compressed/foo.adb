with Ada.Text_IO; use Ada.Text_IO;

procedure Foo is
   function Fact (N : Natural) return Natural is
   begin
      if N <= 1 then
         return 1;
      else
         return N * Fact (N - 1);
      end if;
   end Fact;
begin
   Put_Line ("Fact (6) = " & Natural'Image (Fact (6)));
end Foo;
