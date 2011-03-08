package body Exprs.E_And is

   function Eval (E : Band) return Boolean is
   begin
      return E.A.all.Eval and then E.B.all.Eval; -- # evalAnd
   end;
   
end;
