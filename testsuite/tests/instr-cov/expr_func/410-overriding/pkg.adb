pragma Ada_2012;

package body Pkg is

   overriding function T_Prim (Obj : T_Child; A, B : Boolean) return Boolean
   is (A and then B);

end Pkg;
