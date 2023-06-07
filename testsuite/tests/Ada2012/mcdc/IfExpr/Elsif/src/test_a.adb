with Expr, Support; use Expr, Support;

procedure Test_A is
begin
   Assert (Filter (A => True, B => X, Valt => True) = True);
   Assert (Filter (A => False, B => True, Valt => False) = False);
end;

--# expr.ads
--
--%opts: --trace-mode=bin
--  /eval/ l! ## d!:"B"
--
--%opts: --trace-mode=src
--  /eval/ l! ## dF-:"B"
