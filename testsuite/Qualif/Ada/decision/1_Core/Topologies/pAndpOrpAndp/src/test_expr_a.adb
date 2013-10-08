with Support, Expr; use Support, Expr;

procedure Test_Expr_A is
begin
   Assert (F (False, True, False, False) = False);
   Assert (F (True, True, False, False) = True);
end;

--# expr.adb
--  /eval/  l+ ## 0
--  /retTrue/  l+ ## 0
--  /retFalse/ l+ ## 0
--  /retVal/   l+ ## 0
