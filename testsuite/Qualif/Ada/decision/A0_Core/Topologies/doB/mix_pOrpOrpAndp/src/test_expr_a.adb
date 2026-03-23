with Support, Expr; use Support, Expr;

procedure Test_Expr_A is
begin
   Assert (F (False, False, False, True) = False);
   Assert (F (True, False, False, True) = True);
end;

--# expr.adb
--  /eval/  l+ ## 0
--  /retTrue/  l+ ## 0
--  /retFalse/ l+ ## 0
--  /retVal/   l+ ## 0
