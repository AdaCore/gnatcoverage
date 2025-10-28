with Support, Andthen; use Support, Andthen;

procedure Test_Andthen_T is
begin
   Assert (And_Then (True, True) = True);
end;

--# andthen.adb
--  /andthen/  l! ## oF-
--  /retTrue/  l+ ## 0
--  /retFalse/ l- ## s-
--  /retVal/   l+ ## 0

--  In the IterFilterQuantExpr test, the quantified expression is not the
--  primary decision being assessed, it thus has special expectation which are
--  not fully covered through the various test drivers.
-- /quant_expr_pred/ l! ## dF-
