with Support, Exprs, constructors; use Support, Exprs, Constructors;

procedure Test_And_V1 is
begin
   for X in False .. True loop
      Assert (Expr_And (False, X).Eval = False);
   end loop;
end;

--# exprs-e_and.adb
--  /eval/ l! ## eT-
