with Expr, Support; use Expr, Support;

procedure Test_T is
begin
   Assert (Filter (A => False, Valat => True, Valaf => True,
                   B => True, Valbt => True, Valbf => False) = True);
end;

--# expr.ads
--
--%opts: --trace-mode=bin
--  /eval/ l! ## eF-:"if A", d!:"A", d!:"B"
--
--%opts: --trace-mode=src
--  /eval/ l! ## eF-:"if A", dT-:"A", dF-:"B"
