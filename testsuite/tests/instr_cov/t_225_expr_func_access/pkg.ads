pragma Ada_2022;

package Pkg is

   type Integer_Access is access all Integer;

   A : aliased Integer;
   B : aliased Integer;

   function Get_Ref (Is_A : Boolean) return Integer_Access;
   function Get_Unc_Ref (Is_A : Boolean) return Integer_Access;
   function Get_Unr_Ref (Is_A : Boolean) return Integer_Access;

end Pkg;
