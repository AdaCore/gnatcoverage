with Support, Exprs, Constructors; use Support, Exprs, Constructors;

procedure Test_Andor_Andor_V3 is
begin
   for X in False .. True loop
      Assert (Eval (Expr_Andor_Andor (False, X, True, False, False)) = False);
   end loop;
end;

--# exprs-e_andor.adb
--  /eval/ l! ## eT-
