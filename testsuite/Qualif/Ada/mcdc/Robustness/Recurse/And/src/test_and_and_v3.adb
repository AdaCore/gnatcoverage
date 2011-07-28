with Support, Exprs, Constructors; use Support, Exprs, Constructors;

procedure Test_And_And_V3 is
begin
   Assert (Eval (Expr_And_And (True, True, False)) = False);
end;

--# exprs-e_and.adb
--  /eval/ l! eT-
