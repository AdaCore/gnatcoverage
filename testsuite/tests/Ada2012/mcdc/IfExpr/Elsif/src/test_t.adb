with Expr, Support; use Expr, Support;

procedure Test_T is
begin
   Assert (Filter (A => True, B => X, Valt => True) = True);
   Assert (Filter (A => False, B => True, Valt => True) = True);
   Assert (Filter (A => False, B => False, Valt => False) = True);
end;

--# expr.ads
--  /eval/ l+ ## 0
