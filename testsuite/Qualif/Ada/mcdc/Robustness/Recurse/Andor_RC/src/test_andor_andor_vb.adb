with Support, Exprs, Constructors; use Support, Exprs, Constructors;

procedure Test_Andor_Andor_VB is
begin
   for Xc in False .. True loop
      for Xd in False .. True loop
	 for Xe in False .. True loop
	    Assert (Eval (Expr_Andor_Andor (True, True, Xc, Xd, Xe)) = True);
	 end loop;
      end loop;
   end loop;
end;

--# exprs-e_andor.adb
--  /eval/ l! dF-
