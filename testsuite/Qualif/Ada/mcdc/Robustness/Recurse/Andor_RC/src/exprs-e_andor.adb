package body Exprs.E_Andor is

   function Eval (E : Bandor) return Boolean is
   begin
      return (Eval (E.A) and then Eval(E.B)) or else Eval(E.C); -- # eval
   end;

end;
