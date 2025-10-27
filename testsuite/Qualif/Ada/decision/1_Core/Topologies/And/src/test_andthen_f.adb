with Support, Andthen; use Support, Andthen;

procedure Test_Andthen_F is
begin
   Assert (And_Then (False, True) = False);
   Assert (And_Then (True, False) = False);
end;

--# andthen.adb
--  /andthen/  l! ## oT-
--  /retTrue/  l- ## s-
--  /retFalse/ l+ ## 0
--  /retVal/   l+ ## 0

--  In the IterFilterQuantExpr test, the quantified expression is not the
--  primary decision being assessed, it thus has special expectation which are
--  not fully covered through the various test drivers.
-- /quant_expr_pred/ l! ## d-
