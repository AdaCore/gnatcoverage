pragma Ada_2022;

package body Pkg is

   function Get_Ref (Is_A : Boolean) return Integer_Access
   is (if Is_A then A'Access else B'Access);

   function Get_Unc_Ref (Is_A : Boolean) return Integer_Access
   is (if Is_A then A'Unchecked_Access else B'Unchecked_Access);

   function Get_Unr_Ref (Is_A : Boolean) return Integer_Access
   is (if Is_A then A'Unrestricted_Access else B'Unrestricted_Access);

end Pkg;
