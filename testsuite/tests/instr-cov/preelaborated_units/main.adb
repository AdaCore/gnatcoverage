with Ada.Text_IO; use Ada.Text_IO;

with Pkg; use Pkg;

procedure Main is
begin
   Put_Line ("Fact (1) = " & Integer'Image (Pkg.Fact (1)));
   Pkg.Reset_Coverage;
   Put_Line ("Fact (0) = " & Integer'Image (Pkg.Fact (0)));
   Pkg.Dump_Coverage;
end Main;
