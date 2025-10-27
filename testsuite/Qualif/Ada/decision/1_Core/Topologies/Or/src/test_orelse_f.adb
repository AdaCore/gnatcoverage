with Support, Orelse; use Support, Orelse;

procedure Test_OrElse_F is
begin
   Assert (Or_Else (False, False) = False);
end;

--# orelse.adb
--  /orelse/   l! ## oT-
--  /retTrue/  l- ## s-
--  /retFalse/ l+ ## 0
--  /retVal/   l+ ## 0

--  In the IterFilterQuantExpr test, the quantified expression is not the
--  primary decision being assessed, it thus has special expectation which are
--  not fully covered through the various test drivers.
-- /quant_expr_pred/ l! ## d-
