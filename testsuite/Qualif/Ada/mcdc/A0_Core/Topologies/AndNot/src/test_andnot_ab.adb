with Support, Andnot ; use Support, Andnot;

procedure Test_Andnot_AB is
begin
   Assert (F (True, False) = True);
   Assert (F (True, True) = False);
   Assert (F (False, False) = False);
end;

--# andnot.adb
--  /eval(Stmt|Other)/  l+ ## 0
--  /decisionTrue/  l+ ## 0
--  /decisionFalse/ l+ ## 0
--  /returnValue/   l+ ## 0
--  /decl/   ~l+ ## 0

--  In the IterFilterQuantExpr test, the quantified expression is not the
--  primary decision being assessed, it thus has special expectation which are
--  not fully covered through the various test drivers.
-- /quant_expr_pred/ l! ## dF-
