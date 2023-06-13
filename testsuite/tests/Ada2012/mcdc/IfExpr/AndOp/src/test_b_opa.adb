with Expr, Support; use Expr, Support;

procedure Test_B_Opa is
begin
   --   A op1 B op2 Outer
   --   F T   F T   T
   --   F T   T F   F
   
   Assert (Filter (A => False, Valat => True, Valaf => True,
                   B => False, Valbt => True, Valbf => True) = True);
   
   Assert (Filter (A => False, Valat => False, Valaf => True,
                   B => True, Valbt => False, Valbf => True) = False);
end;

--# expr.ads
--
--%opts: --trace-mode=bin
--  /eval/ l! ## d!:"A", c!:"if A"
--
--%opts: --trace-mode=src
--  /eval/ l! ## dT-:"A", c!:"if A"
