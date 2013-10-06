with Support, Expr; use Support, Expr;

procedure Test_Expr_F is
begin
   Assert (F (False, True) = False);
   Assert (F (True, False) = False);
end;

--# expr.adb
--  /eval/  l! ## oT-
--  /retTrue/  l- ## s-
--  /retFalse/ l+ ## 0
--  /retVal/   l+ ## 0

