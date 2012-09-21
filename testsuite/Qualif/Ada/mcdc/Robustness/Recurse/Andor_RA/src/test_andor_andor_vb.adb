with Support, Exprs, Constructors; use Support, Exprs, Constructors;

procedure Test_Andor_Andor_VB is
begin
   for Xc in False .. True loop
      Assert (Eval (Expr_Andor_Andor (True, True, Xc, False, False)) = False);
   end loop;
end;

--# exprs-e_andor.adb
--  /eval/ l! ## c!:"E.A",c!:"E.C"
