with Support, Exprs, Constructors; use Support, Exprs, Constructors;

procedure Test_And_And_V1 is
begin
   for Xb in False .. True loop
      for Xc in False .. True loop
	 Assert (Eval (Expr_And_And (False, Xb, Xc)) = False);
      end loop;
   end loop;
end;

--# exprs-e_and.adb
--  /eval/ l! ## eT-
