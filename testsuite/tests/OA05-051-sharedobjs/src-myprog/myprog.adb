with Ada.Text_IO; use Ada.Text_IO;

with GNAT.OS_Lib; use GNAT.OS_Lib;

with Mylib1;
with Mylib2;

procedure Myprog is
begin
   Put_Line ("Hello, world!");
   Put_Line ("-> " & Natural'Image (Mylib1.F (1)));
   Put_Line ("-> " & Natural'Image (Mylib2.F (2)));
end Myprog;
