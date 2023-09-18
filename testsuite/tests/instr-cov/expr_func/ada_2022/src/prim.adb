pragma Ada_2022;

package body Prim is

   function Make_Internal (Cond : Boolean) return T is
      (T'(X => (if Cond then 1 else 2)));  -- # expr_dc :o/d:

   function Make_Internal (Cond : Boolean) return TT is
     (TT'(X => 3, Y => 4));  -- # expr_st

end Prim;
