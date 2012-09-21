with Support, Exprs, Constructors; use Support, Exprs, Constructors;

procedure Test_Andor_Andor_V4 is
begin
   for Xb in False .. True loop
      Assert (Eval (Expr_Andor_Andor (False, Xb, True, False, True)) = True);
   end loop;
end;

--# exprs-e_andor.adb
--  /eval/ l! ## eF-
