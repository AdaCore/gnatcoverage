with Ada.Text_IO; use Ada.Text_IO;

with Pkg;       use Pkg;
with Pkg.Child; use Pkg.Child;

procedure Main is
   Dummy : Boolean := Pkg.Opposite (True);
begin
   Put_Line ("Fact (0) = " & Integer'Image (Pkg.Fact (0)));
   Pkg.Do_Nothing_2;
   Pkg.Child.Do_Nothing_3;
end Main;
