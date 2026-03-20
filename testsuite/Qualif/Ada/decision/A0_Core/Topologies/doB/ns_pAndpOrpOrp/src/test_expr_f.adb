with Support, Expr; use Support, Expr;

procedure Test_Expr_F is
begin
   Assert (F (False, False, False, False) = False);
   Assert (F (False, True, False, False) = False);
   Assert (F (True, False, False, False) = False);
end;

--# expr.adb
--  /eval/     l! ## dT-
--  /retTrue/  l- ## s-
--  /retFalse/ l+ ## 0
--  /retVal/   l+ ## 0
