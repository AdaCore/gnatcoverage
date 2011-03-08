package body Exprs.E_Andor is

   function Eval (E : Bandor) return Boolean is
   begin
      return (E.A.Eval and then E.B.Eval) or else E.C.Eval; -- # eval
   end;

end;
