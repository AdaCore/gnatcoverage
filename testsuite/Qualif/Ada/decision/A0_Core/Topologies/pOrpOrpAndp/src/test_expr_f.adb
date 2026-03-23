with Support, Expr; use Support, Expr;

procedure Test_Expr_F is
begin
   Assert (F (False, False, True, False) = False);
   Assert (F (False, False, False, True) = False);
   Assert (F (False, False, False, True) = False);
   Assert (F (False, False, False, False) = False);
end;

--# expr.adb
--  /eval/     l! ## dT-
--  /retTrue/  l- ## s-
--  /retFalse/ l+ ## 0
--  /retVal/   l+ ## 0

--  In the IterFilterQuantExpr test, the quantified expression is not the
--  primary decision being assessed, it thus has special expectation which are
--  not fully covered through the various test drivers.
-- /quant_expr_pred/ l! ## d-
