pragma Ada_2022;

with Ada.Text_IO; use Ada.Text_IO;

procedure Main is

   type Integer_Access is access all Integer;

   A : aliased Integer;
   B : aliased Integer;

   function Get_Ref (Is_A : Boolean) return Integer_Access
   is (if Is_A then A'Access else B'Access);

   function Get_Unc_Ref (Is_A : Boolean) return Integer_Access
   is (if Is_A then A'Unchecked_Access else B'Unchecked_Access);

   function Get_Unr_Ref (Is_A : Boolean) return Integer_Access
   is (if Is_A then A'Unrestricted_Access else B'Unrestricted_Access);

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
