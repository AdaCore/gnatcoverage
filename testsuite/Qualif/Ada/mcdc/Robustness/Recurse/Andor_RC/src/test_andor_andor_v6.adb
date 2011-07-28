with Support, Exprs, Constructors; use Support, Exprs, Constructors;

procedure Test_Andor_Andor_V6 is
begin
   for Xd in False .. True loop
      Assert (Eval (Expr_Andor_Andor (True, False, False, Xd, False)) = False);
   end loop;
end;

--# exprs-e_andor.adb
--  /eval/ l! eT-
