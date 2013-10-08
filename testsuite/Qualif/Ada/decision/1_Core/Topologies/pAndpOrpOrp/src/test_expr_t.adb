with Support, Expr; use Support, Expr;

procedure Test_Expr_T is
begin
   Assert (F (True, False, False, True) = True);
   Assert (F (False, True, True, False) = True);
   Assert (F (False, False, True, False) = True);
   Assert (F (True, True, True, True) = True);
end;

--# expr.adb
--  /eval/     l! ## dF-
--  /retTrue/  l+ ## 0
--  /retFalse/ l- ## s-
--  /retVal/   l+ ## 0
