with Support, Exprs, constructors; use Support, Exprs, Constructors;

procedure Test_And_V3 is
begin
   Assert (Expr_And (True, True).Eval = True);
end;

--# exprs-e_and.adb
--  /evalAnd/ l! dF-
