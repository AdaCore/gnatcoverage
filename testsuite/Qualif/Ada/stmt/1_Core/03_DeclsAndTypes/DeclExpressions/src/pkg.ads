package Pkg is

   function Eval_Decl_Expr (Do_Eval : Boolean) return Boolean;
   --  This function evaluates the declare expression if and only if Do_Eval
   --  is True. In any case, the value of Do_Eval is returned.

end Pkg;
