with Support, Exprs, Constructors; use Support, Exprs, Constructors;

procedure Test_Andor_Andor_V5 is
begin
   for Xb in False .. True loop
      for Xe in False .. True loop
	 Assert (Eval (Expr_Andor_Andor (False, Xb, True, True, Xe)) = True);
      end loop;
   end loop;
end;

--# exprs-e_andor.adb
--  /eval/ l! eF-
