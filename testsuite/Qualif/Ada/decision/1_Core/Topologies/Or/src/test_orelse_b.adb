with Support, Orelse; use Support, Orelse;

procedure Test_OrElse_B is
begin
   Assert (Or_Else (False, False) = False);
   Assert (Or_Else (False, True) = True);
end;

--# orelse.adb
--  /orelse/   l+ ## 0
--  /retTrue/  l+ ## 0
--  /retFalse/ l+ ## 0
--  /retVal/   l+ ## 0

--  In the IterFilterQuantExpr test, the quantified expression is not the
--  primary decision being assessed, it thus has special expectation which are
--  not fully covered through the various test drivers.
-- /quant_expr_pred/ l! ## dF-
