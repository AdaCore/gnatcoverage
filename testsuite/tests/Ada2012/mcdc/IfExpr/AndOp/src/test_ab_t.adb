with Expr, Support; use Expr, Support;

procedure Test_AB_T is
begin
   --   A op1 B op2 Outer
   --   F T   T T   T
   --   T T   F T   T

   Assert (Filter (A => False, Valat => True, Valaf => True,
                   B => True, Valbt => True, Valbf => True) = True);

   Assert (Filter (A => True, Valat => True, Valaf => True,
                   B => False, Valbt => True, Valbf => True) = True);
end;

--# expr.ads
--  /eval/ l! ## eF-:"if A"
