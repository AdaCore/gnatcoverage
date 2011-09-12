
with Support, Expr; use Support, Expr;

procedure Test_Expr_FT is
begin
   If_Orelse (False, True, 5);
   Assert (Expr.Value = 5);

   If_Andthen (False, True, 8);
   Assert (Expr.Value = 5);
end;

--# expr.adb
--  /evalOr/   s=>l+;dum=>l! s=>0, dum=>dF-
--  /latchOr/  l+ 0
--  /evalAnd/  s=>l+;dum=>l! s=>0, dum=>dT-
--  /latchAnd/ l- s-
