with Ada.Text_IO; Use Ada.Text_IO;

with Pkg1;

procedure Disjoint_Main_1 is
begin
   Put_Line ("Pkg1.Fn1 (True) :" & Integer'Image (Pkg1.Fn1 (True)));
   Put_Line ("Pkg1.Fn1 (False):" & Integer'Image (Pkg1.Fn1 (False)));
end Disjoint_Main_1;