with Support, Ornot; use Support, Ornot;

procedure Test_Ornot_T is
begin
   Assert (F (True, False) = True);
   Assert (F (False, False) = True);
   Assert (F (True, True) = True);
end;

--# ornot.adb
--  /eval(Stmt|Other)/   l! ## oF-
--  /decisionTrue/  l+ ## 0
--  /decisionFalse/ l- ## s-
--  /returnValue/   l+ ## 0
--  /decl/   ~l+ ## 0

--  In the IterFilterQuantExpr test, the quantified expression is not the
--  primary decision being assessed, it thus has special expectation which are
--  not fully covered through the various test drivers.
-- /quant_expr_pred/ l! ## dF-
