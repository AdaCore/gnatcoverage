with Support, Exprs, Constructors; use Support, Exprs, Constructors;

procedure Test_Andor_Andor_V7 is
begin
   for Xd in False .. True loop
      Assert (Eval (Expr_Andor_Andor (True, False, False, Xd, True)) = True);
   end loop;
end;

--# exprs-e_andor.adb
--  /eval/ l! eF-
