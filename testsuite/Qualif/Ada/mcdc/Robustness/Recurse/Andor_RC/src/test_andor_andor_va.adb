with Support, Exprs, Constructors; use Support, Exprs, Constructors;

procedure Test_Andor_Andor_VA is
begin
   for Xd in False .. True loop
      Assert (Eval (Expr_Andor_Andor (True, False, True, True, Xd)) = True);
   end loop;
end;

--# exprs-e_andor.adb
--  /eval/ l! ## eF-
