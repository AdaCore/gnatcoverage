with Exprs.E_Val, Exprs.E_And;
use  Exprs.E_Val, Exprs.E_And, Exprs;

package Constructors is

   function Expr_And (A, B : Boolean) return Expr_Ref;
   --  [A and then B] as Band (A, B)

   --    A B  R
   -- ---------
   -- 1  F X  F
   -- 2  T F  F
   -- 3  T T  T

   -- indep(A) = 1,3  indep(B) = 2,3

   function Expr_And_And (A, B, C : Boolean) return Expr_Ref;
   --  Band (A, Band (B, C)), calling for recursive eval without diamond

   --    A  B C  R
   -- ------------
   -- 1  F  X X  F                   (F X)[and.1]
   -- 2  T  F X  F    (F X)[and.1] + (T F)[and.2]
   -- 3  T  T F  F    (T F)[and.2] + (T F)[and.2]
   -- 4  T  T T  T    (T T)[and.3] + (T T)[and.3]

end;
