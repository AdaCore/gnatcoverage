with Ada.Text_IO; use Ada.Text_IO;
with Pkg;         use Pkg;

procedure Main is
   Two_Digits : constant Int_Range := (10, 99);
begin
   if In_Range (9, Two_Digits) then
      Put_Line ("ERROR!");
   elsif not In_Range (10, Two_Digits) then
      Put_Line ("ERROR!");
   end if;
end Main;
