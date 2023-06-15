with Expr, Support; use Expr, Support;

procedure Test_Expr_A is
begin
   Assert (F (False, True, False) = False);
   Assert (F (True, True, False) = True);
end;

--# expr.ads
--  /eval/ l! ## c!:"B",c!:"C"
