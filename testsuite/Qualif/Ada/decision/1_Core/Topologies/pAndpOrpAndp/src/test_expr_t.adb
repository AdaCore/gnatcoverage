with Support, Expr; use Support, Expr;

procedure Test_Expr_T is
begin
   Assert (F (True, False, True, True) = True);
   Assert (F (True, True, True, False) = True);
end;

--# expr.adb
--  /eval/     l! ## dF-
--  /retTrue/  l+ ## 0
--  /retFalse/ l- ## s-
--  /retVal/   l+ ## 0
