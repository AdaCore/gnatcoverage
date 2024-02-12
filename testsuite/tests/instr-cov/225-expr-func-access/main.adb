pragma Ada_2022;

with Ada.Text_IO; use Ada.Text_IO;

with Pkg; use Pkg;

procedure Main is
begin
   Get_Ref (True).all := 1;
   Get_Ref (False).all := 2;

   Get_Unc_Ref (True).all := @ + 10;
   Get_Unc_Ref (False).all := @ + 20;

   Get_Unr_Ref (True).all := @ + 100;
   Get_Unr_Ref (False).all := @ + 200;

   Put_Line (A'Image);
   Put_Line (B'Image);
end Main;
