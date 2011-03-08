with Support, Exprs, Constructors; use Support, Exprs, Constructors;

procedure Test_Andor_Andor_V1 is
begin
   for Xb in False .. True loop
      for Xd in False .. True loop
	 Assert (Eval (Expr_Andor_Andor (False, Xb, False, Xd, False)) = False);
      end loop;
   end loop;
end;

--# exprs-e_andor.adb
--  /eval/ l! dT-
