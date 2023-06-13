with Expr, Support; use Expr, Support;

procedure Test_A_T is
begin
   --   A op1 B op2 Outer
   --   F T   T T   T
   --   T T   T T   T
   
   Assert (Filter (A => False, Valat => True, Valaf => True,
                   B => True, Valbt => True, Valbf => True) = True);
   
   Assert (Filter (A => True, Valat => True, Valaf => True,
                   B => True, Valbt => True, Valbf => True) = True);
end;

--# expr.ads
--
--%opts: --trace-mode=bin
--  /eval/ l! ## eF-:"if A", d!:"B"
--
--%opts: --trace-mode=src
--  /eval/ l! ## eF-:"if A", dF-:"B"
