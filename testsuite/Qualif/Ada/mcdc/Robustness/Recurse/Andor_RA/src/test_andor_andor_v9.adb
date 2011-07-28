with Support, Exprs, Constructors; use Support, Exprs, Constructors;

procedure Test_Andor_Andor_V9 is
begin
   Assert (Eval (Expr_Andor_Andor (True, False, True, False, True)) = True);
end;

--# exprs-e_andor.adb
--  /eval/ l! eF-
