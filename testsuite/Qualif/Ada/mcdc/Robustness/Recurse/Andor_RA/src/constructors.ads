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
   --  [ ({(A and then B) or else C} and then D) or else E]

   --  as Bandor (Bandor (A, B, C), D, E), calling for recursive eval
   --  within diamond

   --    A B C  D E  R
   -- ----------------
   -- 1  F X F  X F  F  andor.1 + andor.1
   -- 2  F X F  X T  T  andor.1 + andor.2

   -- 3  F X T  F F  F  andor.2 + andor.3
   -- 4  F X T  F T  T  andor.2 + andor.4
   -- 5  F X T  T X  T  andor.2 + andor.5

   -- 6  T F F  X F  F  andor.3 + andor.1
   -- 7  T F F  X T  T  andor.3 + andor.2

   -- 8  T F T  F F  F  andor.4 + andor.3
   -- 9  T F T  F T  T  andor.4
   -- A  T F T  T X  T  andor.4 + andor.5

   -- B  T T X  F F  F  andor.5 + andor.3
   -- C  T T X  F T  T  andor.5 + andor.4
   -- D  T T X  T X  T  andor.5

end;
