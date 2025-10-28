pragma Ada_2022;

package body Pkg is

   function Eval_Decl_Expr (Do_Eval : Boolean) return Boolean is
   begin
      return                                          -- # st
        (if Do_Eval                                   -- # st
         then (declare                                -- # st
                  Res : constant Boolean := Do_Eval;  -- # decl
               begin                                  -- # st
                  Res)                                -- # st
         else Do_Eval);                               -- # st
   end Eval_Decl_Expr;

end Pkg;
