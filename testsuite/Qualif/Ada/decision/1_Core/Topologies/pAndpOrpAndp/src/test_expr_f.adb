with Support, Expr; use Support, Expr;

procedure Test_Expr_F is
begin
   Assert (F (True, False, True, False) = False);
   Assert (F (False, True, False, True) = False);
end;

--# expr.adb
--  /eval/     l! ## dT-
--  /retTrue/  l- ## s-
--  /retFalse/ l+ ## 0
--  /retVal/   l+ ## 0
