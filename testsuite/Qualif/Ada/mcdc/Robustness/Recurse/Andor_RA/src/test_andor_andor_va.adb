with Support, Exprs, Constructors; use Support, Exprs, Constructors;

procedure Test_Andor_Andor_VA is
begin
   for Xe in False .. True loop
      Assert (Eval (Expr_Andor_Andor (True, False, True, True, Xe)) = True);
   end loop;
end;

--# exprs-e_andor.adb
--  /eval/ l! dF-
