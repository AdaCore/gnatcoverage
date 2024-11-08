pragma Ada_2022;

package body Pkg is
   function If_Expr_In_Case_Expr (A, B : Boolean) return String is
   begin
      return (case A is                   -- # ret
         when True =>                     -- # case_true
           (if B then "TT" else "TF"),    -- # if_true
         when False => "F.");             -- # case_false
   end If_Expr_In_Case_Expr;
end Pkg;
