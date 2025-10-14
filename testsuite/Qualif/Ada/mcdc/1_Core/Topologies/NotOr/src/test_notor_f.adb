with Support, Notor; use Support, Notor;

procedure Test_Notor_F is
begin
   Assert (F (True, False) = False);
end;

--# notor.adb
--  /eval(Stmt|Other)/   l! ## oT-
--  /decisionTrue/  l- ## s-
--  /decisionFalse/ l+ ## 0
--  /returnValue/   l+ ## 0
--  /decl/   ~l+ ## 0

--  In the IterFilterQuantExpr test, the quantified expression is not the
--  primary decision being assessed, it thus has special expectation which are
--  not fully covered through the various test drivers.
-- /quant_expr_pred/ l! ## d-
