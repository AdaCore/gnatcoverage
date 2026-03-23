with Support, Ornot; use Support, Ornot;

procedure Test_Ornot_A is
begin
   Assert (F (True, True) = True);
   Assert (F (False, True) = False);
end;

--# ornot.adb
--  /eval(Stmt|Other)/   l! ## c!:"B"
--  /decisionTrue/  l+ ## 0
--  /decisionFalse/ l+ ## 0
--  /returnValue/   l+ ## 0
--  /decl/   ~l+ ## 0

--  In the IterFilterQuantExpr test, the quantified expression is not the
--  primary decision being assessed, it thus has special expectation which are
--  not fully covered through the various test drivers.
-- /quant_expr_pred/ l! ## dF-
