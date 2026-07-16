with Ada.Text_IO; use Ada.Text_IO;

with Lib;

procedure Main is
begin
   Lib.I := Lib.I + 10;
   Put_Line (Integer'Image (Lib.I));
end Main;
