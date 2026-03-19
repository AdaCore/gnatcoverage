with Support, Expr; use Support, Expr;

procedure Test_Expr_C is
begin
   Assert (F (True, False, True, False) = True);
   Assert (F (True, False, False, False) = False);
end;

--# expr.adb
--  /eval/  l+ ## 0
--  /retTrue/  l+ ## 0
--  /retFalse/ l+ ## 0
--  /retVal/   l+ ## 0
