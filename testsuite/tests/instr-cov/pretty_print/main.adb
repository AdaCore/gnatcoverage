with Ada.Text_IO; use Ada.Text_IO;
with Pkg;         use Pkg;

procedure Main is
   Who : constant String := "world";
begin
   Put_Line ("Hello, " & Who & "!");
   Put_Line ("Fact (6) = " & Integer'Image (Pkg.Fact (6)));
end Main;
