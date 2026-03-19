with Ada.Text_IO; use Ada.Text_IO;

with Pkg; use Pkg;

procedure Main2 is
begin
   Put_Line ("Fact (0) = " & Integer'Image (Pkg.Fact (0)));
end Main2;
