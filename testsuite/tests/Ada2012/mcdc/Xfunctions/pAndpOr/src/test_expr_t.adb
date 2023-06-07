with Expr, Support; use Expr, Support;

procedure Test_Expr_T is
begin
   Assert (F (True, True, False) = True);
   Assert (F (False, True, True) = True);
   Assert (F (True, False, True) = True);
end;

--# expr.ads
--  /eval/ l! ## eF-
