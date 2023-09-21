pragma Ada_2022;

package Prim is
   type T is tagged record
      X : Integer;
   end record;
   function And_Then (X : T; A, B : Boolean) return Boolean is
      (A and then B);  -- # expr_dc :o/e:
   function Or_Else (X : T; A, B : Boolean) return Boolean is
      (A or else B);   -- # expr_dc :o/e:

   function Make_Internal (Cond : Boolean) return T;

   function Make (Cond : Boolean) return T is (Make_Internal (Cond));  -- # expr_st

   type TT is new T with record
      Y : Integer;
   end record;

   overriding function Make_Internal (Cond : Boolean) return TT;

   overriding function Make (Cond : Boolean) return TT is
      (Make_Internal (Cond));  -- # expr_st

end Prim;
