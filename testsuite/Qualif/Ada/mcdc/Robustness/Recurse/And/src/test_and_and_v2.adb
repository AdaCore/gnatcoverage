with Support, Exprs, Constructors; use Support, Exprs, Constructors;

procedure Test_And_And_V2 is
begin
   for X in False .. True loop
      Assert (Eval (Expr_And_And (True, False, X)) = False);
   end loop;
end;

--# exprs-e_and.adb
--  /evalAnd/ l! dT-
