with Support, Exprs, Constructors; use Support, Exprs, Constructors;

procedure Test_And_And_V4 is
begin
   Assert (Eval (Expr_And_And (True, True, True)) = True);
end;

--# exprs-e_and.adb
--  /eval/ l! ## eF-
