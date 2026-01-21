package body Exprs.E_And is

   function Eval (E : Band) return Boolean is
   begin
      return Eval (E.A.all) and then Eval (E.B.all); -- # eval
   end;

end;
