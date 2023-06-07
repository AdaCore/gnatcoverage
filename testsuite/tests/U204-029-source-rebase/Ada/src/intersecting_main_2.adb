with Ada.Text_IO; Use Ada.Text_IO;

with Pkg1;
with Pkg2;

procedure Intersecting_Main_2 is
begin
   Put_Line ("Pkg1.Fn1 (False):" & Integer'Image (Pkg1.Fn1 (False)));
   Put_Line ("Pkg2.Fn2 (False):" & Integer'Image (Pkg2.Fn2 (False)));
end Intersecting_Main_2;