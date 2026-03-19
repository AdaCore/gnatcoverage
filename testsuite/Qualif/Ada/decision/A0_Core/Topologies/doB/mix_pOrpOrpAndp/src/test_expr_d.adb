with Support, Expr; use Support, Expr;

procedure Test_Expr_D is
begin
   Assert (F (False, False, True, False) = False);
   Assert (F (False, False, True, True) = True);
end;

--# expr.adb
--  /eval/  l+ ## 0
--  /retTrue/  l+ ## 0
--  /retFalse/ l+ ## 0
--  /retVal/   l+ ## 0
