with Support, Exprs, Constructors; use Support, Exprs, Constructors;

procedure Test_And_V2 is
begin
   Assert (Eval (Expr_And (True, False)) = False);
end;

--# exprs-e_and.adb
--  /evalAnd/ l! dT-
