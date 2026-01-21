with Support, Exprs, constructors; use Support, Exprs, Constructors;

procedure Test_And_V3 is
begin
   Assert (Eval (Expr_And (True, True)) = True);
end;

--# exprs-e_and.adb
--  /eval/ l! ## eF-
