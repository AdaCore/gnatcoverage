with Ada.Text_IO; use Ada.Text_IO;
with Gen_Proc;
with Pkg;         use Pkg;
with Proc;

procedure Main is
   Two_Digits : constant Int_Range := (10, 99);

   procedure Proc_Inst is new Gen_Proc;
begin
   Proc;
   Proc_Inst;
   if In_Range (9, Two_Digits) then
      Put_Line ("ERROR!");
   elsif not In_Range (10, Two_Digits) then
      Put_Line ("ERROR!");
   end if;
end Main;
