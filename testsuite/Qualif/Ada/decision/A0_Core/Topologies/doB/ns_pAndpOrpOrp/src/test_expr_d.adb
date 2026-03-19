with Support, Expr; use Support, Expr;

procedure Test_Expr_D is
begin
   Assert (F (False, True, False, True) = True);
   Assert (F (False, True, False, False) = False);
end;

--# expr.adb
--  /eval/  l+ ## 0
--  /retTrue/  l+ ## 0
--  /retFalse/ l+ ## 0
--  /retVal/   l+ ## 0
