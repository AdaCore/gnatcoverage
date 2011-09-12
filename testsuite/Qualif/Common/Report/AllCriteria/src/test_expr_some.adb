
with Support, Expr; use Support, Expr;

procedure Test_Expr_Some is
begin
   -- dT- on orelse, which implies s- on the associated latch stmt

   If_Orelse (False, False, 5);
   Assert (Expr.Value = 0);

   -- c!:"A" on andthen

   If_Andthen (True, True, 7);
   Assert (Expr.Value = 7);

   If_Andthen (True, False, 8);
   Assert (Expr.Value = 7);
end;

--# expr.adb
--  /evalOr/   s=>l+;dum=>l! s=>0, dum=>dT-
--  /latchOr/  l- s-
--  /evalAnd/  s=>l+;dum=>l! s=>0, d=>0, um=>c!:"A "
--  /latchAnd/ l+ 0
