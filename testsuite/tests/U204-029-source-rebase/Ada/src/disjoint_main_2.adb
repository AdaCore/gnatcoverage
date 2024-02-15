with Ada.Text_IO; Use Ada.Text_IO;

with Pkg2;

procedure Disjoint_Main_2 is
begin
   Put_Line ("Pkg2.Fn2 (True) :" & Integer'Image (Pkg2.Fn2 (True)));
   Put_Line ("Pkg2.Fn2 (False):" & Integer'Image (Pkg2.Fn2 (False)));
end Disjoint_Main_2;
