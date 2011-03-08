with Exprs.E_Val, Exprs.E_Andor;
use  Exprs.E_Val, Exprs.E_Andor, Exprs;

package Constructors is

   function Expr_Andor (A, B, C : Boolean) return Expr_Ref;
   --  (A and then B) or else C as Bandor (A, B, C)

   --    A B C  R
   -- -----------
   -- 1  F X F  F
   -- 2  F X T  T
   -- 3  T F F  F
   -- 4  T F T  T
   -- 5  T T X  T

   -- indep(A) = 1,5  indep(B) = 3,5  indep(C) = 3,4 1,2 + 1,4 2,3 (masking)

   function Expr_Andor_Andor (A, B, C, D, E : Boolean) return Expr_Ref;
   --  [(A and then B) or else ((C and then D) or else E]
   --  as Bandor (A, B, Bandor (C, D, E)), calling for recursive eval
   --  within diamond

   --    A B  C D E  R
   -- ----------------
   -- 1  F X  F X F  F
   -- 2  F X  F X T  T
   -- 3  F X  T F F  F
   -- 4  F X  T F T  T
   -- 5  F X  T T X  T

   -- 6  T F  F X F  F
   -- 7  T F  F X T  T
   -- 8  T F  T F F  F
   -- 9  T F  T F T  T
   -- A  T F  T T X  T

   -- B  T T  X X X  T

end;
