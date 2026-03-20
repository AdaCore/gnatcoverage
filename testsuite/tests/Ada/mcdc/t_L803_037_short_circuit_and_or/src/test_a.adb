with Expr, Assert; use Expr;

-- For all the expression evals, arrange to demonstrate independant
-- influence of A only. Expect condition independance not demonstrated
-- diagnostics on other conditions.

procedure Test_A is
begin
   Assert (Bool_And (True, True) = True);
   Assert (Bool_and (False, True) = False);

   Assert (Bool_Or (False, False) = False);
   Assert (Bool_or (True, False) = True);

   Assert (Bool_And_Or (False, True, False) = False);
   Assert (Bool_And_Or (True, True, False) = True);
end;

--# expr.adb
--  /lone-and/ l! ## c!:"B"
--  /lone-or/  l! ## c!:"B"
--  /and-or/   l! ## c!:"B", c!:"C"
