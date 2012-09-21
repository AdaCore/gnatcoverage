with Support, Exprs, Constructors; use Support, Exprs, Constructors;

procedure Test_Andor_Andor_V8 is
begin
   Assert (Eval (Expr_Andor_Andor (True, False, True, False, False)) = False);
end;

--# exprs-e_andor.adb
--  /eval/ l! ## c!:"E.A", c!:"E.B"
