with Support, Andthen; use Support, Andthen;

procedure Test_Andthen_A is
begin
   Assert (And_Then (True, True) = True);
   Assert (And_Then (False, True) = False);
end;

--# andthen.adb
--  /andthen/  l+ ## 0
--  /retTrue/  l+ ## 0
--  /retFalse/ l+ ## 0
--  /retVal/   l+ ## 0

--  In the IterFilterQuantExpr test, the quantified expression is not the
--  primary decision being assessed, it thus has special expectation which are
--  not fully covered through the various test drivers.
-- /quant_expr_pred/ l! ## dF-
