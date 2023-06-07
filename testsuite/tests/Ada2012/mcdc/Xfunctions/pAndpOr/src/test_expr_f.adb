with Expr, Support; use Expr, Support;

procedure Test_Expr_F is
begin
   Assert (F (True, False, False) = False);
   Assert (F (False, True, False) = False);
end;

--# expr.ads
--  /eval/ l! ## eT-
