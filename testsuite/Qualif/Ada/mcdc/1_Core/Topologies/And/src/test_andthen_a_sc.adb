with Support, Andthen; use Support, Andthen;

procedure Test_Andthen_A_SC is
begin
   Assert (And_Then (True, True) = True);
   Assert (And_Then (False, False) = False);
end;

--# andthen.ads andthen.adb
--  /eval(Stmt|Other)/  l! ## c!:"B"
--  /decisionTrue/  l+ ## 0
--  /decisionFalse/ l+ ## 0
--  /returnValue/   l+ ## 0
--  /decl/   ~l+ ## 0

--  In the IterFilterQuantExpr test, the quantified expression is not the
--  primary decision being assessed, it thus has special expectation which are
--  not fully covered through the various test drivers.
-- /quant_expr_pred/ l! ## dF-
