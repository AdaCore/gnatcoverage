with Expr, Support; use Expr, Support;

procedure Test_Opa is
begin
   --   A op1 B op2 Outer
   --   F T   F T   T
   --   F F   F T   F

   Assert (Filter (A => False, Valat => True, Valaf => True,
                   B => False, Valbt => True, Valbf => True) = True);

   Assert (Filter (A => False, Valat => True, Valaf => False,
                   B => False, Valbt => True, Valbf => True) = False);
end;

--# expr.ads
--
--%opts: --trace-mode=bin
--  /eval/ l! ## d!:"A", d!:"B", c!:"if B"
--
--%opts: --trace-mode=src
--  /eval/ l! ## dT-:"A", dT-:"B", c!:"if B"
