with Ada.Text_IO; Use Ada.Text_IO;

with Pkg1;
with Pkg2;

procedure Intersecting_Main_1 is
begin
   Put_Line ("Pkg1.Fn1 (True):" & Integer'Image (Pkg1.Fn1 (True)));
   Put_Line ("Pkg2.Fn2 (True):" & Integer'Image (Pkg2.Fn2 (True)));
end Intersecting_Main_1;
