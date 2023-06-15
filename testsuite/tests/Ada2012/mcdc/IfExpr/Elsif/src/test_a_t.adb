with Expr, Support; use Expr, Support;

procedure Test_A_T is
begin
   Assert (Filter (A => True, B => X, Valt => True) = True);
   Assert (Filter (A => True, B => X, Valt => False) = False);
end;

--# expr.ads
--
--%opts: --trace-mode=bin
--  /eval/ l! ## d!:"if A", d-:"B"
--
--%opts: --trace-mode=src
--  /eval/ l! ## dF-:"if A", d-:"B"
